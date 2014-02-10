;;; Miscellaneous utilities
(define (const x) (lambda _ x))
(define (println x) (print x) (display "\n"))
(define (repr x)
  (with-output-to-string (lambda () (write x))))

(define (map_ f . xs) (apply map f xs) (void))

(define ((partial f . as) . bs) (apply f (append as bs)))
(define ((nary f) . as) (f as))
(define ((unary f) xs) (apply f xs))

(define (foldl1 f xs)
  (match xs
    ['() (error "foldl1 called on empty list")]
    [`(,x) x]
    [(cons x xs) (foldl f x xs)]))

;;; General syntax
(define-syntax le-accum##               ;the ## is for ugliness
  (syntax-rules ()
    [(_ acc e) (letrec acc e)]
    [(_ (acc ...) id ex rest ... e)
      (le-accum## (acc ... (id ex)) rest ... e)]))

(define-syntax le
  (syntax-rules ()
    [(le bindings ... exp)
      (le-accum## () bindings ... exp)]))

(define-syntax-rule (le1 x e body ...) (let ((x e)) body ...))

(define-syntax-rule (matches? exp pat)
  (match exp [pat #t] [_ #f]))

(define-syntax-rule (lambda-rec name rest ...)
  (letrec ((name (lambda rest ...))) name))

(define-syntax-rule (eta f) (lambda x (apply f x)))

;;; Syntax for interfaces
(define-syntax define-interface
  (syntax-rules ()
    [(define-interface iface-name parents method ...)
      (begin
        (define iface-name (interface parents method ...))
        (define-methods iface-name method ...))]))

(define-syntax define-methods
  (syntax-rules ()
    [(define-methods iface-name) (begin)]
    [(define-methods iface-name method methods ...)
      (begin
        (define-method iface-name method)
        (define-methods iface-name methods ...))]))

(define-syntax define-method
  (syntax-rules ()
    [(define-method iface-name (method contract))
      (define-method iface-name method)]
    [(define-method iface-name method)
      (define method
        (let ([g (generic iface-name method)])
          (lambda (object . args)
            (send-generic object g . args))))]))



;;; First we define what our stream interface looks like.
(define-interface stream<%> ()
  read-char   ;; stream -> (or char? eof-object?)
  read-string ;; stream nat -> string?
  location    ;; stream -> loc
  mark        ;; stream -> mark
  restore     ;; stream mark -> ()
  eof?        ;; stream -> bool
  )

(define string-stream%
  (class* object% (stream<%>)
    (init string)

    (define contents string)
    (define current-pos 0)
    (super-new)

    (define/public (read-char)
      (if (eof?) eof
        (begin0
          (string-ref contents current-pos)
          (set! current-pos (+ 1 current-pos)))))

    (define/public (read-string amt)
      (let ([start current-pos]
            [end (min (+ current-pos amt) (string-length contents))])
        (set! current-pos end)
        (substring contents start end)))

    (define/public (location) current-pos)
    (define/public (mark) current-pos)

    (define/public (restore pos)
      (and (< pos 0) (> pos (string-length contents))
        (error "invalid position"))
      (set! current-pos pos))

    (define/public (eof?)
      (= current-pos (string-length contents)))

    (define/public (get-contents) contents)
    (define/public (read-all)
      (read-string (- (string-length contents) current-pos)))))

(define (string-stream s)
  (new string-stream% [string s]))

;; TODO: file-stream%


;;; Monoids, represented explicitly.


;;; A parser is a function that takes:
;;; - the extensible environment (a monoid of some sort)
;;; - an input stream
;;; - a hard failure continuation
;;; - a soft failure continuation
;;; - a success continuation
;;;
;;; The success continuation takes:
;;; - a boolean which is #t iff we consumed input
;;; - the result
;;;
;;; The failure continuations take:
;;; - a location
;;; - an error message
;;;
;;; This approach is strongly inspired by Haskell's Parsec library. Most of the
;;; definitions are my own, but some (eg. many1, skip-many1) are straight ports.

(define (parse-string parser string)
  (parser (void) (string-stream string)
    (lambda (loc msg) (raise `(hard ,loc ,msg)))
    (lambda (loc msg) (raise `(soft ,loc ,msg)))
    (lambda (_ r) r)))

(define (pretty-parse-string parser string)
  (let ([str (string-stream string)])
    (match (parser (void) str
             (lambda (loc msg) `(hard ,loc ,msg))
             (lambda (loc msg) `(soft ,loc ,msg))
             (lambda (_ r) `(ok ,r)))
      [`(ok ,r) (display "ok: ") (println r)]
      [`(,mode ,loc ,msg)
        (display mode)
        (display " failure at ")
        (print loc)
        (displayln (string-append ": " msg))])
    (send str read-all)))

;;; Basic monadic operations
(define ((return x) env str hardk softk ok) (ok #f x))

(define ((fail msg) env str hardk softk ok)
  (softk (location str) ))

(define ((fmap1 f a) env str hardk softk ok)
  (a env str hardk softk
    (lambda (ate res) (ok ate (f res)))))

(define ((fmap2 f a b) env str hardk softk ok)
  (a env str hardk softk
    (lambda (aate ares)
      (b env str hardk (if aate hardk softk)
        (lambda (bate bres)
          (ok (or aate bate) (f ares bres)))))))

(define (lift1 f) (partial fmap1 f))
(define (lift2 f) (partial fmap2 f))

(define (seq ps) (foldr (lift2 cons) (return '()) ps))
(define list* (nary seq))
(define (<$> f . ks) (fmap1 (unary f) (seq ks)))

(define >>=
  (case-lambda
    [(x) x]
    [(a f . fs)
      (lambda (env str hardk softk ok)
        (a env str hardk softk
          (lambda (ate res)
            ((apply >>= (f res) fs)
              env str hardk (if ate hardk softk) ok))))]))

(define (*> . as) (<$> last (seq as)))
(define (<* . as) (<$> car (seq as)))
(define (<$ x . as) (apply <* (return x) as))


;;; Special monadic operations: try, ask, local
(define ((try p) env str hardk softk ok)
  (p env str softk softk ok))

;;; Choice
(define ((psum ps) env str hardk softk ok)
  (let ([savepoint (mark str)])
    (match ps
      ['() (softk (location str) "empty psum")]
      [(cons x xs)
        (define (f x xs)
          (x env str hardk
            (lambda (loc msg)
              (match xs
                ['() (softk loc msg)]
                [(cons x xs)
                  (restore str savepoint)
                  (f x xs)]))
            ok))
        (f x xs)])))

(define choice (nary psum))

(define ((look-ahead p) env str hardk softk ok)
  (let ([savepoint (mark str)])
    (p env str hardk softk
      (lambda (ate res)
        (restore str savepoint)
        (ok ate res)))))

(define (pzero env str hardk softk ok)
  (softk (location str) "pzero called"))

(define (peof env str hardk softk ok)
  (if (eof? str) (ok #f (void))
    (softk (location str) "expected EOF")))


;;; Useful combinators
(define (option x p) (choice p (return x)))
(define (optional p) (option (void) p))

(define (many p) (option '() (many1 p)))
(define (many1 p) (<$> cons p (eta (many p))))
(define (skip-many p) (optional (skip-many1 p)))
(define (skip-many1 p) (*> p (eta (skip-many p))))

(define (sep-by p sep) (option '() (sep-by1 p sep)))
(define (sep-by1 p sep) (<$> cons p (many (*> sep p))))
;; separated and ended by sep
(define (end-by p sep) (many (<* p sep)))
(define (end-by1 p sep) (many1 (<* p sep)))
;; separated and *optionally* ended by sep
(define (sep-end-by p sep) (option '() (sep-end-by1 p sep)))
(define (sep-end-by1 p sep)
  (<$> cons p (option '() (*> sep (eta (sep-end-by p sep))))))

(define (between pre post x) (*> pre (<* x post)))

(define ((pfilter parser msg pred) env str hardk softk ok)
  (let ([loc (location str)])
    (parser env str hardk softk
      (lambda (ate res)
        (if (pred res) (ok ate res)
          ((if ate hardk softk) loc msg))))))

(define (negate p) (optional (*> p pzero)))
(define (not-followed-by p pafter) (<* p (negate pafter)))


;;; Useful primitives
(define (string expect)
  (let ([len (string-length expect)])
    (if (= 0 len) (return (void))
      (lambda (env str hardk softk ok)
        (let ([loc (location str)]
              [got (read-string str len)])
          (if (string=? got expect) (ok #t expect)
            (hardk loc
              (string-append
                "expected " (repr expect)
                ", got " (repr got)))))))))

(define (any-char env str hardk softk ok)
  (let ([c (read-char str)])
    (if (eof-object? c) (softk (location str) "unexpected EOF")
      (ok #t c))))

(define (any-of s)
  (let ([l (string->list s)])
    (try (pfilter any-char (string-append "expected any of " (repr s))
           (lambda (c) (member c l))))))

(define (none-of s)
  (let ([l (string->list s)])
    (try (pfilter any-char (string-append "expected none of " (repr s))
           (lambda (c) (not (member c l)))))))

(define (parens x) (between (any-of "(") (any-of ")") x))

(define alpha (any-of "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"))
(define digit (any-of "0123456789"))
(define alphanum (choice alpha digit))

(define (string-of p) (<$> list->string (many p)))
(define (string-of1 p) (<$> list->string (many1 p)))

(define space (any-of " \r\n\t\v\f"))
(define spaces (skip-many space))
(define spaces1 (skip-many1 space))


;;; Parser for a simple ML-like surface syntax (w/out infix operators, type
;;; definitions, mutually recursive definitions). All parsers consume all
;;; subsequent whitespace. If this were a serious parser we'd use a separate
;;; tokenizer and avoid this annoyance.
;;;
;;; TODO: pattern-matching, data constructors.
(define reserved-words
  (map string->symbol (string-split "let val fun fn in")))

(define (spaced p) (<* p spaces))
(define (token s) (spaced (try (string s))))
(define (keyword s) (spaced (try (not-followed-by (string s) alphanum))))
(define (labeled lbl . ps) (<$> (partial cons lbl) (seq ps)))
(define (keyworded kwd . ps)
  (*> (keyword (symbol->string kwd))
      (apply labeled kwd ps)))

(define p-ident
  (spaced
    (try (pfilter (<$> string->symbol
                    (string-of1 (choice alphanum (any-of "_"))))
           "cannot use keyword as identifier"
           (lambda (x) (not (member x reserved-words)))))))

(define p-num
  (spaced (<$> (compose string->number list->string append)
            (option '() (list* (any-of "-")))
            (many1 digit))))

(define p-string
  (spaced (between (any-of "\"") (any-of "\"")
            (string-of (choice (none-of "\\\"")
                               (*> (any-of "\\") any-char))))))

(define p-atom (choice p-num p-string p-ident))

(define p-expr
  (<$> (lambda (es) (foldl1 (lambda (e acc) `(app ,acc ,e)) es))
    (eta p-exprs1)))

(define p-simple-expr (choice (spaced (parens p-expr)) p-atom))

(define p-lambda
  (keyworded 'fn
    p-ident
    (*> (token "=>") p-expr)))

(define p-decl
  (choice
    (keyworded 'val p-ident (*> (token "=") p-expr))
    (keyworded 'fun p-ident (many1 p-ident) (*> (token "=") p-expr))
    (keyworded 'data (many1 p-ident))))

(define p-let (keyworded 'let (many p-decl) (*> (keyword "in") p-expr)))

(define p-exprs1
  (choice (list* (choice p-let p-lambda))
    (<$> cons p-simple-expr (eta p-exprs))))

(define p-exprs (choice p-exprs1 (<$ '() spaces)))


;;; Evaluator for this ML-like language.
(define (ml-eval e env)
  (match e
    [`(let ,decls ,exp)
      ;; Extend context
      (ml-eval exp (ml-eval-decls decls env))]
    [`(app ,f ,a) ((ml-eval f env) (ml-eval a env))]
    [`(fn ,p ,e)
      (lambda (a)
        (ml-eval e `((,p ,a) ,@env)))]
    [(? symbol?) (lookup e env)]
    [(? string?) e]
    [(? number?) e]
    [_ (error "I don't know how to evaluate that.")]))

(define (lookup x env) (cadr (assoc x env))) ;TODO: useful error message

(define (ml-eval-decls decls env) (foldl ml-eval-decl env decls))

(define (ml-eval-decl decl env)
  (match decl
    [`(val ,i ,e) `((,i ,(ml-eval e env)) ,@env)]
    [`(fun ,i (,p . ,ps) ,e)
      (letrec ((f (lambda (a)
                    (ml-eval
                      (foldr (lambda (p e) `(fn ,p ,e)) e ps)
                      `((,p ,a) (,i ,f) ,@env)))))
        `((,i ,f) ,@env))]
    [_ (error "I don't know how to evaluate that.")]))


;;; TODO: Compiler (to Racket) for this ML-like language


(displayln "loaded parse-monoid.rkt")

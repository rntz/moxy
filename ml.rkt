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


;;; Naïve parser combinators (like eg. Parsec, but much less efficient).
;;;
;;; A parser is a function that takes:
;;; - an input stream
;;; - a failure continuation
;;; - a success continuation
;;;
;;; The success continuation takes the result
;;; The failure continuation takes no arguments

(define debug-parser #f)

(define (parse-string parser string)
  (let ([str (string-stream string)])
    (parser str
      ;; not actually guaranteed to get us the location of the "real" failure.
      (lambda () (error (string-append "failed at " (repr (location str)))))
      identity)))

;;; Basic monadic operations
(define ((return x) str fk ok) (ok x))

;;; TODO: remove if unused
(define ((fail msg) str fk ok)
  (when debug-parser
    (displayln (string-append (repr (location str)) msg)))
  (fk))

(define ((fmap1 f a) str fk ok)
  (a str fk (compose ok f)))

(define ((fmap2 f a b) str fk ok)
  (a str fk (lambda (ares) (b str fk (lambda (bres) (ok (f ares bres)))))))

(define (lift1 f) (partial fmap1 f))
(define (lift2 f) (partial fmap2 f))

(define (seq ps) (foldr (lift2 cons) (return '()) ps))
(define list* (nary seq))
(define (<$> f . ks) (fmap1 (unary f) (seq ks)))

(define >>=
  (case-lambda
    [(x) x]
    [(a f . fs)
      (lambda (str fk ok)
        (a str fk
          (lambda (res)
            ((apply >>= (f res) fs) str fk ok))))]))

(define (*> . as) (<$> last (seq as)))
(define (<* . as) (<$> car (seq as)))
(define (<$ x . as) (apply <* (return x) as))


;;; Choice
(define ((psum ps) str fk ok)
  (let ([savepoint (mark str)])
    (define (f ps)
      (if (null? ps) (fk)
        ((car ps) str
          (lambda () (restore str savepoint) (f (cdr ps)))
          ok)))
    (f ps)))

(define choice (nary psum))

(define ((look-ahead p) str fk ok)
  (let ([savepoint (mark str)])
    (p str fk
      (lambda (res)
        (restore str savepoint)
        (ok res)))))

(define pzero (fail "pzero called"))

(define (peof str fk ok)
  (if (eof? str) (ok (void)) (fk)))


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

(define ((pfilter pred parser) str fk ok)
  (let ([loc (location str)])
    (parser str fk
      (lambda (res)
        (if (pred res) (ok res) (fk))))))

(define (negate p) (optional (*> p pzero)))
(define (not-followed-by p pafter) (<* p (negate pafter)))


;;; Useful primitives
(define (string expect)
  (let ([len (string-length expect)])
    (if (= 0 len) (return (void))
      (lambda (str fk ok)
        (let ([loc (location str)]
              [got (read-string str len)])
          (if (string=? got expect) (ok expect) (fk)))))))

(define (any-char str fk ok)
  (let ([c (read-char str)])
    (if (eof-object? c) (fk) (ok c))))

(define (any-of s)
  (let ([l (string->list s)])
    (pfilter (lambda (c) (member c l)) any-char)))

(define (none-of s)
  (let ([l (string->list s)])
    (pfilter (lambda (c) (not (member c l))) any-char)))

(define (parens x) (between (any-of "(") (any-of ")") x))

(define alpha (any-of "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"))
(define digit (any-of "0123456789"))
(define alphanum (choice alpha digit))

(define (string-of p) (<$> list->string (many p)))
(define (string-of1 p) (<$> list->string (many1 p)))

(define space (any-of " \r\n\t\v\f"))
(define spaces (skip-many space))
(define spaces1 (skip-many1 space))


;;; Parser for a simple ML-like surface syntax (w/out infix, types, mutually
;;; recursive definitions). All parsers consume all subsequent whitespace. If
;;; this were a serious parser we'd use a separate tokenizer and avoid this
;;; annoyance.
;;;
;;; TODO: pattern-matching
(define reserved-words
  (map string->symbol (string-split "case data fn fun in let of val")))

(define (spaced p) (<* p spaces))
(define (token s) (spaced (string s)))
(define (keyword s) (spaced (not-followed-by (string s) alphanum)))
(define (labeled lbl . ps) (<$> (partial cons lbl) (seq ps)))
(define (keyworded kwd . ps)
  (*> (keyword (symbol->string kwd))
      (apply labeled kwd ps)))

(define p-ident
  (spaced
    (pfilter (lambda (x) (not (member x reserved-words)))
      (<$> (compose string->symbol list->string append)
        (list* (choice alpha (any-of "_")))
        (many (choice alphanum (any-of "_")))))))

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

(define p-decl
  (choice
    (keyworded 'val p-ident (*> (token "=") p-expr))
    (keyworded 'fun p-ident (many1 p-ident) (*> (token "=") p-expr))
    (keyworded 'data (many1 p-ident))))

(define p-let (keyworded 'let (many p-decl) (*> (keyword "in") p-expr)))

(define p-lambda (keyworded 'fn p-ident (*> (token "=>") p-expr)))

(define p-pattern (many1 p-ident))
(define p-case-branch (list* p-pattern (*> (token "=>") p-expr)))
(define p-case (keyworded 'case p-expr
                 (*> (keyword "of")
                     (sep-by1 p-case-branch (token "|")))))

(define p-exprs1
  (choice (list* (choice p-case p-lambda p-let))
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
      (letrec ([f (lambda (a)
                    (ml-eval
                      (foldr (lambda (p e) `(fn ,p ,e)) e ps)
                      `((,p ,a) (,i ,f) ,@env)))])
        `((,i ,f) ,@env))]
    ;; [`(data (,i . ,params))
    ;;   (let ([uid (gensym i)]
    ;;         [ctor (if (null? params) (list uid)
    ;;                 (foldl
    ;;                   (lambda (args) (cons uid args))
    ;;                   params
    ;;                   ))]))
    ;;   ]
    [_ (error "I don't know how to evaluate that.")]))


;;; TODO: Compiler (to Racket) for this ML-like language


(displayln "loaded parse-monoid.rkt")

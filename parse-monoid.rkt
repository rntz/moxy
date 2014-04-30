(require racket/stream)
(require racket/sequence)

;;; Miscellaneous utilities
(define (const x) (lambda _ x))
(define (println x) (print x) (display "\n"))
(define (repr x)
  (with-output-to-string (lambda () (write x))))

(define (map_ f . xs) (apply map f xs) (void))

(define ((partial f . as) . bs) (apply f (append as bs)))
(define ((nary f) . as) (f as))
(define ((unary f) xs) (apply f xs))

(define list->number (compose string->number list->string))
(define list->symbol (compose string->symbol list->string))

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
  empty?      ;; stream -> bool
  peek        ;; stream -> element
  read-one    ;; stream -> element
  read-string ;; stream nat -> (seq element)
  location    ;; stream -> loc
  mark        ;; stream -> mark
  restore     ;; stream mark -> ()
  )

(define string-stream%
  (class* object% (stream<%>)
    (init string)

    (define contents string)
    (define current-pos 0)
    (super-new)

    (define/public (empty?)
      (= current-pos (string-length contents)))

    (define/public (peek)
      (when (empty?) (error "tried to peek at empty string-stream%"))
      (string-ref contents current-pos))

    (define/public (read-one)
      (when (empty?) (error "tried to read-one from end of string-stream%"))
      (begin0
        (string-ref contents current-pos)
        (set! current-pos (+ 1 current-pos))))

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

    (define/public (get-contents) contents)
    (define/public (read-all)
      (read-string (- (string-length contents) current-pos)))))

(define (string-stream s)
  (new string-stream% [string s]))

;; TODO: file-stream%

(define stream-stream%
  (class* object% (stream<%>)
    (init stream)

    (define contents stream)
    (super-new)

    (define/public (empty?) (stream-empty? contents))

    (define/public (peek)
      (when (empty?) (error "tried to peek at empty stream-stream%"))
      (stream-first contents))

    (define/public (read-one)
      (when (empty?) (error "tried to read-one from end of stream-stream%"))
      (begin0
        (stream-first contents)
        (set! contents (stream-tail contents))))

    (define/public (read-string n)
      (if (empty?) '()
        (cons (read-one) (read (- n 1)))))

    (define/public (location)
      ;; TODO: location tracking.
      #f)

    (define/public (mark) contents)
    (define/public (restore mrk) (set! contents mrk))

    (define/public (read-all)
      (begin0
        (stream->list contents)
        (set! contents empty-stream)))))


;;; Monoids, represented explicitly.


;;; A parser is a function that takes:
;;; - the extensible environment (a value of some monoid)
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
;;; This is basically a reimplementation of the Haskell Parsec library.

(define (parse-string parser string)
  (let ([str (string-stream string)])
    (values
      (parser (void) str
        (lambda (loc msg) `(hard ,loc ,msg))
        (lambda (loc msg) `(soft ,loc ,msg))
        (lambda (_ r) `(ok ,r)))
      (send str read-all))))

;;; Basic monadic operations
(define ((return x) env str hardk softk ok) (ok #f x))

(define ((fail msg) env str hardk softk ok)
  (softk (location str) msg))

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
;; ((lift2 f) a b) == (fmap2 f a b)

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


;;; Special monadic operations: try, ask, local

;;; Runs a parser p, turning hard failures into soft failures. This allows
;;; nontrivial backtracking, which is useful but can cause asymptotic slowdown.
(define ((try p) env str hardk softk ok)
  (p env str softk softk ok))

;;; Returns the current environment.
(define ((ask) env str hardk softk ok) (ok #f env))

;;; Runs parser p in environment altered by f.
(define ((local f p) env str hardk softk ok)
  (p (f env) str hardk softk ok))

;;; Choice. Returns the result of the first succeeding parser. Backtracks and
;;; chooses the next parser from the list on soft failure. Propagates hard
;;; failure.
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

;;; Parser that always fails soft.
(define (pzero env str hardk softk ok)
  (softk (location str) "pzero called"))

;;; Parser that expects end-of-input.
(define (peof env str hardk softk ok)
  (if (empty? str) (ok #f (void))
    (softk (location str) "expected EOF")))


;;; Useful combinators
(define (option x p) (choice p (return x)))
(define (optional p) (option (void) p))

(define (many p) (option '() (many1 p)))
(define (many1 p) (<$> cons p (eta (many p))))
(define (skip-many p) (optional (skip-many1 p)))
(define (skip-many1 p) (*> p (eta (skip-many p))))
(define (str-many p) (<$> list->string (many p)))
(define (str-many1 p) (<$> list->string (many1 p)))

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

(define ((pfilter parser msgf pred) env str hardk softk ok)
  (let ([loc (location str)])
    (parser env str hardk softk
      (lambda (ate res)
        (if (pred res) (ok ate res)
          ((if ate hardk softk) loc (msgf res)))))))


;;; Useful primitives
(define (take n)
  (if (= 0 n) (return (void))
    (lambda (env str hardk softk ok)
      (let ([got (read-string str n)])
        (ok (< 0 (sequence-length got)) got)))))

(define (expect seq [test equal?])
  (pfilter (take (sequence-length seq))
    (lambda (got) (string-append "expected " (repr seq) ", got" (repr got)))
    (partial test seq)))

(define (token s) (try (expect s)))     ;TODO: remove?

(define (take-one env str hardk softk ok)
  (if (empty? str) (softk (location str) "unexpected EOF")
    (ok #t (read-one str))))

(define (peek-one env str hardk softk ok)
  (if (empty? str) (softk (location str) "unexpected EOF")
    (ok #f (peek str))))

(define (satisfy p [msgf (lambda (c) (string-append "unexpected " (repr c)))])
  (try (pfilter take-one msgf p)))

(define (any-of s [test equal?])
  (satisfy (lambda (c) (sequence-ormap (partial test c) s))
    (lambda (c) (string-append "expected one of " (repr s)))))

(define (none-of s [test equal?])
  (satisfy (lambda (c) (not (sequence-ormap (partial test c) s)))
    (lambda (c) (string-append "expected none of " (repr s)))))

(define (parens x) (between (token "(") (token ")") x))

(define alpha (any-of "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"))
(define digit (any-of "0123456789"))

(define space (any-of " \r\n\t\v\f"))
(define whitespace (skip-many1 space))
(define opt-whitespace (skip-many space))


(displayln "loaded parse-monoid.rkt")

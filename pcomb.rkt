#lang racket

(require racket/stream)
(require racket/sequence)

(require "util.rkt")
(require "values.rkt")

(provide
  return fail pmap1 pmap2 lift1 lift2 seq seq* lift pdo
  >>= <* *> <$> <*>
  try ask local
  psum choice peof
  option optional option-maybe
  many many1 skip-many skip-many1 str-many str-many1
  sep-by sep-by1 sep-by1 end-by end-by1 sep-end-by sep-end-by1
  begin-sep-by begin-sep-by1
  begin-sep-end-by begin-sep-end-by1
  between pmap-maybe pfilter
  take expect-seq take-one try-one-maybe expect peek-one satisfy any-of none-of
  ;; alpha digit space whitespace opt-whitespace
  )

;;; Miscellaneous utilities
(define list->number (compose string->number list->string))
(define list->symbol (compose string->symbol list->string))


;;; A parser is a function that takes:
;;; - an environment
;;; - an input stream
;;; - a hard failure continuation
;;; - a soft failure continuation
;;; - a success continuation
;;;
;;; The success continuation takes:
;;; - the rest of the stream
;;; - a boolean which is #t iff we consumed input
;;; - the result
;;;
;;; The failure continuations take:
;;; - the stream (at the location of the error)
;;; - an error message
;;;
;;; This is basically a reimplementation of the Haskell Parsec library.

;; some contracts
(define parser/c
  (-> any/c stream? procedure? procedure? procedure? any/c))

;;; Basic monadic operations
(define/contract ((return x) env s hardk softk ok)
  (-> any/c parser/c)
  (ok s #f x))

(define ((fail msg) env s hardk softk ok)
  (softk s msg))

(define/contract ((pmap1 f a) env s hardk softk ok)
  (-> procedure? parser/c parser/c)
  (a env s hardk softk
    (lambda (s ate res) (ok s ate (f res)))))

(define/contract ((pmap2 f a b) env s hardk softk ok)
  (-> (-> any/c any/c any/c) parser/c parser/c parser/c)
  (a env s hardk softk
    (lambda (s aate ares)
      (b env s hardk (if aate hardk softk)
        (lambda (s bate bres)
          (ok s (or aate bate) (f ares bres)))))))

(define (lift1 f) (curry pmap1 f))
(define (lift2 f) (curry pmap2 f))
;; ((lift2 f) a b) == (pmap2 f a b)

;; horrendously inefficient
(define (seq ps) (foldr (lift2 cons) (return '()) ps))
(define seq* (nary seq))
(define (<$> f . ks) (pmap1 (unary f) (seq ks)))
(define (lift f) (curry <$> f))
(define (<*> f . ks) (apply <$> funcall f ks))

(define >>=
  (case-lambda
    [(x) x]
    [(a f . fs)
      (lambda (env s hardk softk ok)
        (a env s hardk softk
          (lambda (s ate res)
            ((apply >>= (f res) fs)
              env s hardk (if ate hardk softk) ok))))]))

(define (*> . as) (<$> last (seq as)))
(define (<* . as) (<$> car (seq as)))

(define-syntax pdo
  (syntax-rules (<- let define)
    [(_ p) p]
    [(_ let x expr body ...) (match-let ((x expr)) (pdo body ...))]
    [(_ define x expr body ...) (begin (define x expr) (pdo body ...))]
    [(_ pat <- expr body ...) (>>= expr (match-lambda [pat (pdo body ...)]))]
    [(_ expr body ...) (*> expr (pdo body ...))]))


;;; Special monadic operations: try, ask, local

;;; Runs a parser p, turning hard failures into soft failures. This allows
;;; nontrivial backtracking, which is useful but can cause asymptotic slowdown.
(define ((try p) env s hardk softk ok)
  (p env s softk softk ok))

;;; Returns the current environment.
(define (ask env s hardk softk ok) (ok s #f env))
(define (asks f) (pmap1 f ask))

;;; Runs parser p in environment altered by f.
(define ((local f p) env s hardk softk ok)
  (p (f env) s hardk softk ok))

;;; Choice. Returns the result of the first succeeding parser. Backtracks and
;;; chooses the next parser from the list on soft failure. Propagates hard
;;; failure.
(define ((choice/2 a b) env s hardk softk ok)
  (a env s hardk (lambda _ (b env s hardk softk ok)) ok))

(define/match (psum ps)
  [('()) (fail "empty psum")]
  [((list x)) x]
  [((cons x xs)) (choice/2 x (psum xs))])
(define choice (nary psum))

;;; Parser that expects end-of-input.
(define (peof env s hardk softk ok)
  (if (stream-empty? s) (ok s #f (void)) (softk s "expected EOF")))


;;; Useful combinators
(define (option x p) (choice p (return x)))
(define (optional p) (option (void) p))
(define (option-maybe p) (option None (<$> Just p)))

;;; Q: won't this overflow stack during parsing of a long list?
;;; A: no, it'll just fill up the heap with continuation closures.
;;; and that's the magic of CPS!
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
;; separated and optionally begun by sep.
(define (begin-sep-by p sep) (*> (optional sep) (sep-by p sep)))
(define (begin-sep-by1 p sep) (*> (optional sep) (sep-by1 p sep)))

;; Sequence of `p', separated by `sep', optionally begun and/or ended by `sep'.
;; `p' and `sep' better not be inter-ambiguous.
(define (begin-sep-end-by p sep) (*> (optional sep) (sep-end-by p sep)))
(define (begin-sep-end-by1 p sep) (*> (optional sep) (sep-end-by1 p sep)))

(define (between pre post x) (*> pre (<* x post)))

;; Parser a, (a -> Maybe b), a -> String -> Parser b
;;
;; (pmap-maybe p f m) runs p, calling f on its result x. If f returns (Just r),
;; we return r. Otherwise we fail, calling (m x) to generate an error message.
;;
;; Consumes input iff p consumes.
(define ((pmap-maybe parser func msgf) env s-orig hardk softk ok)
  (parser env s-orig hardk softk
    (lambda (s ate res)
      (match (func res)
        [(Just x) (ok s ate x)]
        [(None) ((if ate hardk softk) s-orig (msgf res))]))))

(define (pfilter parser pred msgf)
  (pmap-maybe parser (lambda (x) (if (pred x) (Just x) None)) msgf))


;;; Useful primitives
(define ((take n) env s hardk softk ok)
  (define got (stream-take s n))
  (ok (stream-tail rest n) (null? got) got))

(define (expect-seq seq [test equal?])
  (try
    (pfilter (take (sequence-length seq))
      (curry test seq)
      (lambda (got)
        (string-append "expected " (repr seq) ", got" (repr got))))))

(define (take-one env s hardk softk ok)
  (if (stream-empty? s) (softk s "unexpected EOF")
    (ok (stream-rest s) #t (stream-first s))))

(define (expect t [test equal?])
  (try (pfilter take-one
         (curry test t)
         (lambda (got)
           (string-append "expected " (repr t) ", got " (repr got))))))

(define (peek-one env s hardk softk ok)
  (if (stream-empty? s) (softk s "unexpected EOF")
    (ok s #f (stream-first s))))

(define (satisfy p [msgf (lambda (t) (format "unexpected ~v" t))])
  (try (pfilter take-one p msgf)))

(define (try-one-maybe f [msgf (lambda (t) (format "unexpected ~v" t))])
  (try (pmap-maybe take-one f msgf)))

(define (any-of s [test equal?])
  (satisfy (lambda (c) (sequence-ormap (curry test c) s))
    (lambda (c) (string-append "expected one of " (repr s)))))

(define (none-of s [test equal?])
  (satisfy (lambda (c) (not (sequence-ormap (curry test c) s)))
    (lambda (c) (string-append "expected none of " (repr s)))))

;; (define alpha (any-of "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"))
;; (define digit (any-of "0123456789"))

;; (define space (any-of " \r\n\t\v\f"))
;; (define whitespace (skip-many1 space))
;; (define opt-whitespace (skip-many space))

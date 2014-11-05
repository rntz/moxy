#lang racket

(require racket/stream)
(require racket/sequence)

(require "util.rkt")
(require "values.rkt")

(provide
  string-stream stream-stream
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
        (set! contents (stream-tail contents 1))))

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

(define (stream-stream s) (new stream-stream% [stream s]))


;;; A parser is a function that takes:
;;; - an environment
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

;; some contracts
(define parser/c
  (-> any/c any/c procedure? procedure? procedure? any/c))

;;; Basic monadic operations
(define/contract ((return x) env str hardk softk ok)
  (-> any/c parser/c)
  (ok #f x))

(define ((fail msg) env str hardk softk ok)
  (softk (location str) msg))

(define/contract ((pmap1 f a) env str hardk softk ok)
  (-> procedure? parser/c parser/c)
  (unless (procedure? f) (error 'pmap1 "not a procedure")) ;TODO: contracts
  (unless (procedure? a) (error 'pmap1 "not a parser"))
  (a env str hardk softk
    (lambda (ate res) (ok ate (f res)))))

(define/contract ((pmap2 f a b) env str hardk softk ok)
  (-> (-> any/c any/c any/c) parser/c parser/c parser/c)
  (unless (procedure? f) (error 'pmap2 "not a procedure")) ;TODO: contracts
  (unless (procedure? a) (error 'pmap2 "not a parser"))
  (unless (procedure? b) (error 'pmap2 "not a parser"))
  (a env str hardk softk
    (lambda (aate ares)
      (b env str hardk (if aate hardk softk)
        (lambda (bate bres)
          (ok (or aate bate) (f ares bres)))))))

(define (lift1 f) (curry pmap1 f))
(define (lift2 f) (curry pmap2 f))
;; ((lift2 f) a b) == (pmap2 f a b)

(define (seq ps) (foldr (lift2 cons) (return '()) ps))
(define seq* (nary seq))
(define (<$> f . ks) (pmap1 (unary f) (seq ks)))
(define (lift f) (curry <$> f))
(define (<*> f . ks) (apply <$> (lambda (x . as) (apply x as)) f ks))

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
(define ((try p) env str hardk softk ok)
  (p env str softk softk ok))

;;; Returns the current environment.
(define (ask env str hardk softk ok) (ok #f env))
(define (asks f) (pmap1 f ask))

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
        (let loop ([x x] [xs xs])
          (x env str hardk
            (lambda (loc msg)
              (match xs
                ['() (softk loc msg)]
                [(cons x xs)
                  (restore str savepoint)
                  (loop x xs)]))
            ok))])))

(define choice (nary psum))

;;; Parser that expects end-of-input.
(define (peof env str hardk softk ok)
  (if (empty? str) (ok #f (void))
    (softk (location str) "expected EOF")))


;;; Useful combinators
(define (option x p) (choice p (return x)))
(define (optional p) (option (void) p))
(define (option-maybe p) (option None (<$> Just p)))

;;; Q: won't this overflow stack during parsing of long list?
;;; A: no, it'll just fill up the heap with continuation closures.
;;; that's the magic of CPS!
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
(define ((pmap-maybe parser func msgf) env str hardk softk ok)
  (let ([loc (location str)])
    (parser env str hardk softk
      (lambda (ate res)
        (match (func res)
          [(Just x) (ok ate x)]
          [(None) ((if ate hardk softk) loc (msgf res))])))))

(define (pfilter parser pred msgf)
  (pmap-maybe parser (lambda (x) (if (pred x) (Just x) None)) msgf))


;;; Useful primitives
(define (take n)
  (if (= 0 n) (return (void))
    (lambda (env str hardk softk ok)
      (let ([got (read-string str n)])
        (ok (< 0 (sequence-length got)) got)))))

(define (expect-seq seq [test equal?])
  (try
    (pfilter (take (sequence-length seq))
      (curry test seq)
      (lambda (got)
        (string-append "expected " (repr seq) ", got" (repr got))))))

(define (take-one env str hardk softk ok)
  (if (empty? str) (softk (location str) "unexpected EOF")
    (ok #t (read-one str))))

(define (expect t [test equal?])
  (try (pfilter take-one
         (curry test t)
         (lambda (got)
           (string-append "expected " (repr t) ", got " (repr got))))))

(define (peek-one env str hardk softk ok)
  (if (empty? str) (softk (location str) "unexpected EOF")
    (ok #f (peek str))))

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

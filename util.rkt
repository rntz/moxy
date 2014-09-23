#lang racket

(provide
  const map_ flip iter funcall call-with partial nary unary
  zip-with zip
  mkid mktemp
  matches? lambda-rec eta
  define-interface
  foldl1 reduce dict-union hash-unions
  show repr printfln)

(define (const x) (lambda _ x))
(define ((flip f) x y) (f y x))
(define (iter n f) (apply compose (build-list n (const f))))
(define (funcall f . as) (apply f as))
(define ((call-with . as) f) (apply f as))

(define (map_ f . xs) (apply map f xs) (void))

(define ((partial f . as) . bs) (apply f (append as bs)))
(define ((nary f) . as) (f as))
(define ((unary f) xs) (apply f xs))

;; zips arbitrary sequences, not just lists
(define (zip-with f xs ys)
  (for/list ([x xs] [y ys]) (f x y)))

(define (zip xs ys) (zip-with list xs ys))

(define (mkid fmt . args) (gensym (apply format (format "~a." fmt) args)))
(define (mktemp fmt . args) (gensym (apply format (format "_~a." fmt) args)))


;;; Some syntactic help
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


;; Data structure manipulations
;; not sure I need this
(define (foldl1 f xs)
  (match xs
    ['() (error "foldl1 called on empty list")]
    [`(,x) x]
    [(cons x xs) (foldl f x xs)]))

;; Reduces a list using a monoid
(define (reduce list identity function)
  (match list
    ['() identity]
    [`(,a) a]
    [`(,a ,b) (function a b)]
    [(cons a as) (foldl (flip function) a as)]))

(define (dict-union a b [combine (lambda (x y) y)])
  (cond
    [(dict-empty? a) b]
    [(dict-empty? b) a]
    [else
      ;; TODO: this is inefficient
      (dict-for-each b
        (lambda (key val)
          (set! a (dict-set a key
                    (if (dict-has-key? a key)
                     (combine (dict-ref a key) val)
                      val)))))
      a]))

(define (hash-unions hashes [combine (lambda (x y) y)])
  (if (null? hashes) (hash)
    ;; relies on reduce not using its second argument if list is non-empty
    (reduce hashes (void) (lambda (x y) (dict-union x y combine)))))


;; String formatting & IO

;; (show x) shows x in human-readable form, with some nice abbreviations.
;; (repr x) shows x in (read)-able form, without any nice abbreviations.
;;
;;  (repr '(quote foo)) --> "(quote foo)"
;;  (show '(quote foo)) --> "'foo"
;;
;; Neither of them unnecessarily prefixes things with quotes, as print does by
;; default. Neither will drop quotes around strings, as display does.
(define (show x) (call-with-output-string (lambda (p) (print x p 1))))
(define (repr x) (with-output-to-string (lambda () (write x))))

(define (printfln fmt . args)
  (apply printf fmt args)
  (newline))

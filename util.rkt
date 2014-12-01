#lang racket

(require (for-syntax syntax/parse))
(require racket/stream)

(provide
  const flip iter funcall call-with curry nary unary
  zip-with zip
  mkid mktemp
  define-syntax-parser define-many
  eta
  reduce stream-take ;; foldl1 dict-union hash-unions
  show repr printfln)

(define (const x) (lambda _ x))
(define ((flip f) x y) (f y x))
(define (iter n f) (apply compose (build-list n (const f))))
(define (funcall f . as) (apply f as))
(define ((call-with . as) f) (apply f as))

(define ((curry f . as) . bs) (apply f (append as bs)))
(define ((nary f) . as) (f as))
(define ((unary f) xs) (apply f xs))

;; zips arbitrary sequences; always outputs a list
(define (zip-with f xs ys) (for/list ([x xs] [y ys]) (f x y)))
(define (zip xs ys) (zip-with list xs ys))

(define (mkid fmt . args) (gensym (apply format (format "~a." fmt) args)))
(define (mktemp fmt . args) (gensym (apply format (format "_~a." fmt) args)))


;;; Some syntactic help
(define-syntax-rule (eta f) (lambda x (apply f x)))

;;; Metasyntactic help
(define-syntax define-syntax-parser
  (syntax-parser
    [(_ name:id rest ...)
      #'(define-syntax name (syntax-parser rest ...))]
    [(_ (name:id args ...) body ...)
      #'(define-syntax-parser name [(_ args ...) body ...])]))

(define-syntax-parser syntax-magic-apply
  [(_ func arg:id) #'(func arg)]
  [(_ func (arg ...)) #'(func arg ...)])

(define-syntax-rule (define-many definer arg ...)
  (begin (syntax-magic-apply definer arg) ...))


;; Data structure manipulations
;; (define (foldl1 f xs)
;;   (match xs
;;     ['() (error "foldl1 called on empty list")]
;;     [`(,x) x]
;;     [(cons x xs) (foldl f x xs)]))

;; Reduces a list using a monoid
(define (reduce list identity function)
  (match list
    ['() identity]
    [`(,a) a]
    [`(,a ,b) (function a b)]
    [(cons a as) (foldl (flip function) a as)]))

(define (stream-take s n)
  (if (= n 0) empty-stream
    (cons (stream-first s) (stream-take (stream-rest s) (- n 1)))))


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

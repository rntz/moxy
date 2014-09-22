#lang racket

(require "util.rkt")
(require (only-in "core-forms.rkt" expr:lit))
;; TODO: Move expr:call to core-forms.rkt
(require (only-in "parse-builtins.rkt" expr:call))

(provide
  pure lift fmap ap ;; applicative
  quasi unquo quo   ;; quasiquoting
  seq               ;; utilities
  run-quasi)        ;; using the damn thing

(define ((run-at n) x) (x n))

;; type (Q a) represents a quasiquoting computation returning an a.
;;
;; A quasiquoting computation is implemented as a 1-argument function. It takes
;; a natural number n indicating the quasiquoting depth to interpret it at, and
;; return the appropriate expression at that depth, ostensibly of type (IR^n a).

;; run-quasi : Q a -> a
(define (run-quasi q) (q 0))

;; pure : a -> Q a
(define ((pure x) n) ((iter n expr:lit) x))

;; lift : (a1, a2, ..., an -> b) -> (Q a1, Q a2, ..., Q an -> Q b)
(define (((lift f) . as) n)
  (apply
    ((iter n lift-once) f)
    (map (run-at n) as)))

(define ((lift-once f) . as) (expr:call (expr:lit f) as))

(define (fmap f . as) (apply (lift f) as))
(define (ap f . as) (apply (lift (lambda (f . as) (apply f as))) f as))

;; is this really right?
;; I'm not convinced.
(define ((quasi k) n) (k (+ n 1)))
(define ((unquo k) n)
  (if (> n 0) (k (- n 1))
    (error "unquote underflow!")))

(define (quo k) (fmap expr:lit k))

(define (seq as) (apply fmap list as))
(define seq* (nary seq))

;;; for testing
(require "env.rkt")                     ;expr-compile
(define test
  (quasi
    (fmap expr:call
      (pure (expr:lit +))
      (seq* (pure (expr:lit 1)) (pure (expr:lit 2))))))

(define (run-test)
  (eval (expr-compile (run-quasi test) #f)))

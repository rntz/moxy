#lang racket

(require "util.rkt")
(require "pcomb.rkt")

(provide quasi-base^ quasi^ quasi@)

;; A signature for the base monad that we provide utilities to operate under.
;;
;; Basically, it has to be a (Reader Int)-like monad: you can ask for and
;; locally rebind an integer value.
(define-signature quasi-base^
  (pure
    ;; we expect fmap and ap to be n-ary.
    fmap ap bind join                   ; monad resources
    ask                                 ; m int
    local                               ; (int -> int) -> m unit

    lit                                 ; a -> exp a
    ;; call : exp, exp... -> exp
    ;; takes expressions for
    call
    ))

;; A signature for the interface to quasiquotation that we provide.
(define-signature quasi^
  (pure fmap ap
   quo unquo quasi))

;; Implements quasi^ using quasi-base^
(define-unit quasi@
  (import (prefix base- quasi-base^))
  (export quasi^)

  (define (pure x)
    (base-fmap (lambda (n) ((iter n base-lit) x)) base-ask))

  (define (lift-once f) (partial base-call (base-lit f)))

  (define (fmap f . as)
    (base-bind base-ask
      (lambda (n)
        (apply base-fmap ((iter n lift-once) f) as))))

  (define (ap f . as)
    (apply fmap (lambda (f . as) (apply f as)) f as))

  (define (quo k) (fmap base-lit k))

  (define (quasi k)
    (base-local (lambda (x) (+ 1 x)) k))

  (define (unquo k)
    (base-local
      (lambda (x) (if (> x 0) (- x 1)
               (error 'unquo "unquote underflow")))
      k)))

#lang racket

(require "util.rkt")

(provide
  extension-point make-extension-point
  extension-point-name extension-point-empty extension-point-join
  define-extension-point
  env-empty env-join env-make env-get)

(struct extension-point (uid name empty join)
  #:constructor-name make-extension-point-internal
  #:methods
  gen:equal+hash
  [(define (equal-proc a b equal?-recur)
     (eq? (extension-point-uid a) (extension-point-uid b)))
   (define (hash-proc a hash-recur)
     (hash-recur (extension-point-uid a)))
    (define (hash2-proc a hash-recur)
      (hash-recur (extension-point-uid a)))])

(define (make-extension-point name empty join)
  (make-extension-point-internal (gensym name) name empty join))

(define-syntax-rule (define-extension-point name empty join)
  (define name (make-extension-point (quote name) empty join)))

;; Environments are immutable hashtables, mapping extension-points to their
;; monoid values. If an extension-point is absent, it is the same as being

;; mapped to its empty value.
(define env-empty (hash))

(define (env-join . es)
  (hash-unions es extension-point-join))

(define (env-make ext-point value)
  (hash ext-point value))

(define (env-get ext ext-point)
  (dict-ref ext ext-point (extension-point-empty ext-point)))

(displayln "env.rkt loaded")

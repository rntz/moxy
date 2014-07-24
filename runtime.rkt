#lang racket

(require "env.rkt")

;; This is a crude hack but it works, so whatever. Ideally we'd expose only the
;; set of language primitives we actually need, but racket's baroque module and
;; namespace system makes that frustratingly complicated.

(provide make-runtime)

;; Returns (values resolve-env namespace)
(define (make-runtime)
  (let ([ns (make-base-namespace)])
    (parameterize ([current-namespace ns])
      (namespace-require "values.rkt"))
    (values env-empty ns)))

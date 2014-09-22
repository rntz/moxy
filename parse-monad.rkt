#lang racket

(require
  "util.rkt"
  "quasi-unit.rkt"
  (rename-in "pcomb.rkt"
    (ask p-ask) (local p-local)))

(provide parse-quasi-monad@)

(define-struct parse-local (env quote-depth)
  #:prefab)

(define-unit parse-quasi-monad@
  (import)
  (export quasi-monad^)

  (define pure return)
  (define fmap <$>)
  (define ap <*>)
  (define bind >>=)
  (define (join f) (>>= f identity))

  ;; whee, manual lensing
  (define ask (<$> parse-local-quote-depth p-ask))
  (define (local f p)
    (p-local
      (lambda (x)
        (struct-copy parse-local x
          [quote-depth (f (parse-local-quote-depth x))]))
      p)))

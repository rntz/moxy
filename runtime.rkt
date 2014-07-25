#lang racket

(require (for-syntax syntax/parse))
(require syntax/parse)

(require "util.rkt")
(require "values.rkt")
(require "env.rkt")

;; This is a crude hack but it works, so whatever. Ideally we'd expose only the
;; set of language primitives we actually need, but racket's baroque module and
;; namespace system makes that frustratingly complicated.

(provide make-runtime)

(define-syntax (define-names stx)
  (syntax-parse stx
    [(_ env definitions ...)
      (syntax-parse #'(definitions ...)
        [() #'env]
        [(definition definitions ...)
          #`(define-name definition
              (define-names env definitions ...))])]))

(define-syntax (define-name stx)
  (syntax-parse stx
    [(_ definition env)
      (syntax-parse #'definition
        [x:id #'(define-name [x x] env)]
        [(name:id value)
          #`(begin
              (namespace-set-variable-value! 'name value)
              (env-join env (env-single @vars (@vars-var 'name 'name))))]
        [(#:tag tag-id:id ctor-id:id)
          #`(begin
              (namespace-set-variable-value! 'tag-id tag-id)
              (namespace-set-variable-value! 'ctor-id ctor-id)
              (env-join env (env-single @vars
                              (@vars-ctor
                                (tag-name tag-id)
                                ctor-id
                                tag-id
                                (tag-arity tag-id)))))])]))

;; Returns (values resolve-env namespace)
(define (make-runtime)
  (let* ([ns (make-base-namespace)]
         [env (parameterize ([current-namespace ns])
                (namespace-require "values.rkt")
                (namespace-require "env.rkt")
                (define-names env-empty
                  + - / *
                  [== equal?]
                  ;; TODO: string-append

                  format
                  [say (lambda (fmt . args)
                         (printf fmt args)
                         (display "\n"))]
                  [print (lambda (x) (write x) (display "\n"))]

                  [#:tag tag:Just Just]
                  [#:tag tag:None None]
                  maybe
                  [fromMaybe from-maybe]
                  [maybeMap maybe-map]
                  [maybeFilter maybe-filter]

                  [#:tag tag:Monoid Monoid]
                  [#:tag tag:ExtPoint ExtPoint]

                  [emptyHash hash-empty]
                  [hashSingle hash-single]
                  [hashHas hash-has?]
                  [hashLookup hash-lookup] [hashGet hash-get]
                  [hashPut hash-put] [hashPutWith hash-put-with]
                  [hashDelete hash-delete]
                  [hashAlter hash-alter]
                  [hashMap hash-map]
                  [hashUnion hash-union]))])
    (values env ns)))

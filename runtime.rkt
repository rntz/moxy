#lang racket

(require (for-syntax syntax/parse))

(require "debug.rkt")
(require "util.rkt")
(require "values.rkt")
(require "env.rkt")
(require "core-forms.rkt")              ;@vars-var

;; This is a crude hack but it works, so whatever. Ideally we'd expose only the
;; set of language primitives we actually need, but racket's baroque module and
;; namespace system makes that frustratingly complicated.

(provide
  make-runtime
  engine engine? make-engine engine-namespace engine-builtin-resolve-env)

;; we need the namespace to pass to 'eval.
;; we need builtin-resolve-env when we go to load a new file: it needs to see
;; only the builtin bindings.
(define-struct engine (namespace builtin-resolve-env)
  #:prefab)

(define-namespace-anchor anchor)
(define anchor-ns (namespace-anchor->empty-namespace anchor))

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
              (namespace-set-variable-value! 'name value #t)
              (env-join env (env-single @vars (@vars-var 'name 'name))))]
        [(#:tag tag-id:id ctor-id:id)
          #`(begin
              (namespace-set-variable-value! 'tag-id tag-id #t)
              (namespace-set-variable-value! 'ctor-id ctor-id #t)
              (env-join env (env-single @vars
                              (@vars-ctor
                                (tag-name tag-id)
                                ctor-id
                                tag-id
                                (match (tag-arity tag-id)
                                  [0 None]
                                  [n (Just n)])))))])]))

;; Returns (values resolve-env namespace)
(define (make-runtime)
  (let* ([ns (make-base-namespace)]
         [env (parameterize ([current-namespace ns])
                ;; Attach existing modules to the namespace so can reuse them.
                ;; This prevents it re-creating all the tags we've defined,
                ;; which leads to weird bugs like:
                ;;
                ;;     - if N then 0 else 1;
                ;;     0
                ;;     - # wtf?
                ;;
                ;; FIXME: fails if our current directory isn't where values.rkt
                ;; etc. are! :( :( :(
                (namespace-attach-module anchor-ns "values.rkt")
                (namespace-attach-module anchor-ns "env.rkt")
                (namespace-require "values.rkt")
                (namespace-require "env.rkt")
                (define-names env-empty
                  ;; TODO: string-append, string comparison

                  [debug toggle-debug!]

                  format
                  [say printfln]
                  [print (lambda (x) (write x) (display "\n"))]

                  [#:tag tag:True True]
                  [#:tag tag:False False]
                  [not    (compose truthify falsey?)]
                  [toBool (compose truthify truthy?)]

                  [#:tag tag:L L]
                  [#:tag tag:R R]

                  [#:tag tag:Just Just]
                  [#:tag tag:None None]
                  maybe
                  [fromMaybe from-maybe]
                  [maybeMap maybe-map]
                  [maybeFilter
                    (lambda (v ok?) (maybe-filter v (compose truthy? ok?)))]

                  [#:tag tag:Monoid Monoid]
                  [#:tag tag:ExtPoint ExtPoint]

                  [symbol string->symbol]
                  [string (lambda (x) (format "~a" x))]

                  ;; TODO: a module containing the extension points, parser
                  ;; combinators, etcetera

                  [emptyHash hash-empty]
                  [hashIsEmpty (compose truthify hash-empty?)]
                  [hashSize hash-count]
                  [hashSingle hash-single]
                  [hashHas (compose truthify hash-has?)]
                  [hashLookup hash-lookup] [hashGet hash-get]
                  [hashPut hash-put] [hashPutWith hash-put-with]
                  [hashDelete hash-delete]
                  [hashAlter hash-alter]
                  [hashMap hash-map]
                  [hashUnion hash-union]))])
    (values env (make-engine ns env))))

#lang racket

(require (for-syntax syntax/parse))

(require "debug.rkt")
(require "util.rkt")
(require "values.rkt")
(require "objects.rkt")
(require "env.rkt")
(require "engine.rkt")
(require "core-forms.rkt")              ;@vars-var
(require (only-in "parse-builtins.rkt" builtin-parse-env))

(provide new-engine)

(define (vars x) (list env-empty (env-single @vars x)))

(define (val name value)
  (let ((id (gensym name)))
    (namespace-set-variable-value! id value #t)
    (vars (@vars-var name id))))

(define/contract (tag tag ctor)
  (-> tag? any/c (list/c hash? hash?))
  (let* ([name (tag-name tag)]
         [tag-id (mkid "tag:~a" name)]
         [ctor-id (mkid name)])
    (namespace-set-variable-value! tag-id tag #t)
    (namespace-set-variable-value! ctor-id ctor #t)
    (vars (@vars-ctor name ctor-id tag-id
            (match (tag-arity tag)
              [0 None]
              [n (Just (tag-fields tag))])))))

(define (nodule name envlist)
  (define parse-env (env-join* (map car envlist)))
  (define resolve-env (env-join* (map cadr envlist)))
  (list
    (env-single @nodules
      (@nodules-nodule name
        (record
          [resolveExt resolve-env]
          [parseExt parse-env])))
    env-empty))

(define-syntax (mkenv stx)
  (with-syntax ([(_ d) stx])
    (syntax-parse #'d
      [x:id #'(val 'x x)]
      [(name:id value) #'(val 'name value)]
      [(#:tag tag-id ctor-id) #'(tag tag-id ctor-id)]
      [(#:nodule name:id defs ...)
        #'(nodule 'name (mkenv* defs ...))])))

(define-syntax (mkenv* stx)
  (syntax-parse stx
    [(_) #''()]
    [(_ d ds ...)
      #'(cons (mkenv d) (mkenv* ds ...))]))

(define (make-env)
  (mkenv*
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

    [racketEval eval]
    [eval (lambda (e) (eval (expr-compile e env-empty)))]

    ;; TODO: a module containing the extension points, parser
    ;; combinators, etcetera

    (#:nodule Hash
      [empty hash-empty]
      [isEmpty (compose truthify hash-empty?)]
      [size hash-count]
      [single hash-single]
      [has (compose truthify hash-has?)]
      [lookup hash-lookup]
      [get hash-get]
      [put hash-put]
      [putWith hash-put-with]
      [delete hash-delete]
      [alter hash-alter]
      [map hash-map]
      [union hash-union])

    (#:nodule Moxy
      [exprs @exprs]
      [infixExprs @infixes]
      [pats @pats]
      [infixPats @infix-pats]
      [decls @decls]
      [tops @tops]
      [modules @nodules]
      [vars @vars]
      [quoteForms @quote-forms])
    ))

;; This is a crude hack but it works, so whatever. Ideally we'd expose only the
;; set of language primitives we actually need, but racket's baroque module and
;; namespace system makes that frustratingly complicated.

(define-namespace-anchor anchor)
(define anchor-ns (namespace-anchor->empty-namespace anchor))

(define (new-engine)
  (define ns (make-base-namespace))
  (define envlist
    (parameterize ([current-namespace ns])
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
      (make-env)))
  (define parse-env (env-join builtin-parse-env (env-join* (map car envlist))))
  (define resolve-env (env-join* (map cadr envlist)))
  (make-engine ns parse-env resolve-env))

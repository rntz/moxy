#lang racket

(require (for-syntax syntax/parse))

(require "debug.rkt")
(require "util.rkt")
(require "tags.rkt")
(require "values.rkt")
(require "objects.rkt")
(require "lex.rkt")                     ;for the token tags
(require "env.rkt")
(require "engine.rkt")
(require "core-forms.rkt")              ;@vars-var
(require "pcomb.rkt")
(require "parse.rkt")
(require "parse-builtins.rkt")
(require (prefix-in q- "quasi.rkt"))

(provide new-engine)

(define/match (envpair-join/2 a b)
  [((list ax ay) (list bx by))
    (list (env-join ax bx) (env-join ay by))])

(define (envpair-join* l) (reduce l (list env-empty env-empty) envpair-join/2))
(define envpair-join (nary envpair-join*))

(define (vars x) (list env-empty (env-single @vars x)))

(define (env-val name value)
  (let ((id (gensym name)))
    (namespace-set-variable-value! id value #t)
    (vars (@vars-var name id))))

(define/contract (env-tag tag ctor fields)
  (-> tag? any/c (listof symbol?) (list/c hash? hash?))
  (let* ([name (tag-name tag)]
         [tag-id (mkid "tag:~a" name)]
         [ctor-id (mkid name)])
    (namespace-set-variable-value! tag-id tag #t)
    (namespace-set-variable-value! ctor-id ctor #t)
    (vars (@vars-ctor name ctor-id tag-id
            (if (= 0 (tag-arity tag)) None (Just fields))))))

(define (env-nodule name envs)
  (match-define (list parse-env resolve-env) envs)
  (list
    (env-single @nodules
      (@nodules-nodule name
        (record
          [resolveExt resolve-env]
          [parseExt parse-env])))
    env-empty))

(define-syntax-parser mkenv
  [(_ x:id) #'(env-val 'x x)]
  [(_ (name:id value)) #'(env-val 'name value)]
  ;; TODO: variadic #:tag
  [(_ (#:tag name:id))
    #`(env-tag #,(tag-name-id #'name) name '#,(tag-fields #'name))]
  [(_ (#:nodule name:id defs ...))
    #'(env-nodule 'name (mkenv defs ...))]
  [(_ part ...) #'(envpair-join (mkenv part) ...)])

(define (make-env)
  (mkenv
    ;; TODO: string-append, string comparison

    [debug toggle-debug!]

    format
    [say printfln]
    [print (lambda (x) (write x) (display "\n"))]

    [#:tag True]
    [#:tag False]
    [not    (compose truthify falsey?)]
    [toBool (compose truthify truthy?)]

    [#:tag L]
    [#:tag R]

    [#:tag Just]
    [#:tag None]
    maybe
    [fromMaybe from-maybe]
    [maybeMap maybe-map]
    [maybeFilter
      (lambda (v ok?) (maybe-filter v (compose truthy? ok?)))]

    [#:tag Monoid]
    [#:tag ExtPoint]

    [symbol string->symbol]
    gensym
    [string (lambda (x) (format "~a" x))]

    [racketEval eval]
    [eval (lambda (e) (eval (expr-compile e env-empty)))]

    ;; convenience
    [sexp (lambda (x) ((hash-get 'sexp x)))]
    [add (lambda (x y) (+ x y))]

    (#:nodule List
      [nil '()] cons list
      null append [concat (lambda (x) (append* x))]
      [build build-list]
      map
      [filter (lambda (p l) (filter (compose truthy? p) l))]
      foldr foldl
      [head car] [tail cdr])

    (#:nodule Set
      [empty (set)]
      [has (lambda (elem set) (truthify (set-member? set elem)))]
      [fromList list->set]
      [toList set->list]
      [union set-union]
      [intersect set-intersect])

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
      [union hash-union]
      [unions hash-unions]
      ;; XXX lists
      [keys hash-keys]
      [values hash-values]
      [fromList hash-from-list]
      [fromKeysValues hash-from-keys-values])

    (#:nodule AST
      [mkId mkid] [mkTemp mktemp]
      [exprVar expr:var]
      [exprLit expr:lit]
      [exprLambda expr:lambda]
      [exprRacket expr:racket]
      [patVar pat:var]
      [declBegin decl:begin]
      [varLocal var:local]
      )

    (#:nodule Ext
      [Exprs @exprs]
      [InfixExprs @infix-exprs]
      [Pats @pats]
      [InfixPats @infix-pats]
      [Decls @decls]
      [Tops @tops]
      [Modules @nodules]
      [Vars @vars]
      [QuoteForms @quote-forms])

    (#:nodule Env
      [empty env-empty]
      [join env-join]
      [joins env-join*]
      [single env-single]
      [get env-get])

    (#:nodule Quasi
      [pure q-pure] [lift q-lift] [map q-fmap] [ap q-ap]
      [quasi q-quasi] [unquo q-unquo] [quo q-quo]
      [seq q-seq] [list q-seq*]
      [run q-run])

    (#:nodule Parse
      ;; from pcomb.rkt
      [pure return] lift [map <$>] [ap <*>] [bind >>=]
      [join (lambda (k) (>>= k identity))]
      fail
      try ask local
      psum choice peof
      option optional [optionMaybe option-maybe]
      ;; these all return racket lists
      many many1
      [skipMany skip-many] [skipMany1 skip-many1]
      [sepBy sep-by] [sepBy1 sep-by1]
      [endBy end-by] [endBy1 end-by1]
      [sepEndBy sep-end-by] [sepEndBy1 sep-end-by1]
      [beginSepBy begin-sep-by] [beginSepBy1 begin-sep-by1]
      [beginSepEndBy begin-sep-end-by] [beginSepEndBy1 begin-sep-end-by1]
      between
      [mapByMaybe pmap-maybe]
      ;; [filterBy pfilter] ;; need to adapt for booleans
      ;; take ;; returns a racket list
      expect
      [takeOne take-one]
      [satisfy (lambda (p . as) (apply satisfy (compose truthy? p) as))]
      [tryOneMaybe try-one-maybe]
      [anyOf any-of] [noneOf none-of]

      ;; from parse.rkt
      [localEnv local-env]
      keyword keysym comma dot semi colon equals bar
      lparen rparen lbrace rbrace lbrack rbrack
      parens braces brackets
      [string p-str] [number p-num] [literal p-lit]
      [id p-id] [anyId p-any-id] [varId p-var-id] [capsId p-caps-id]
      ;; TODO: p-qualified, p-var return a list

      [expr p-expr] [exprAt p-expr-at] [atomicExpr p-atomic-expr]
      [prefixExpr p-prefix-expr] [infixExpr p-infix-expr]

      [pat p-pat] [patAt p-pat-at] [atomicPat p-atomic-pat]
      [prefixPat p-prefix-pat] [infixPat p-infix-pat]

      ;; from parse-builtins.rkt
      [unquoExpr p-unquo-expr]

      ;; ah, whatever
      listish
      [listishQ q-listish]
      [qIfy q-ify]                      ;uch

      ;; TODO: decl etc. return lists!
      ;; [decl p-decl] [decls p-decls]

      ;; TODO: parse-eval & co
      )

    (#:nodule Lex
      [#:tag TLPAREN] [#:tag TRPAREN]
      [#:tag TLBRACK] [#:tag TRBRACK]
      [#:tag TLBRACE] [#:tag TRBRACE]
      [#:tag TID] [#:tag TSYM]
      [#:tag TNUM] [#:tag TSTR])
    ))

;; This is a crude hack but it works, so whatever. Ideally we'd expose only the
;; set of language primitives we actually need, but racket's baroque module and
;; namespace system makes that frustratingly complicated.

(define-namespace-anchor anchor)
(define anchor-ns (namespace-anchor->empty-namespace anchor))

(define (new-engine)
  (define ns (make-base-namespace))
  (match-define (list parse-env resolve-env)
    (parameterize ([current-namespace ns])
      ;; Attach existing modules to the namespace so can reuse them.
      ;; This prevents it re-creating all the tags we've defined,
      ;; which leads to weird bugs like:
      ;;
      ;;     - if False then 0 else 1;
      ;;     0
      ;;     - # wtf?
      ;;
      ;; FIXME: fails if our current directory isn't where values.rkt
      ;; etc. are! :( :( :(
      (namespace-attach-module anchor-ns "tags.rkt")
      (namespace-attach-module anchor-ns "values.rkt")
      (namespace-attach-module anchor-ns "env.rkt")
      (namespace-require "tags.rkt")
      (namespace-require "values.rkt")
      (namespace-require "env.rkt")
      (make-env)))
  (make-engine ns (env-join builtin-parse-env parse-env) resolve-env))

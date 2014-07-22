#lang racket

(require (for-syntax racket/syntax))    ;format-id
(require syntax/parse (for-syntax syntax/parse))

(require "util.rkt")
(require "values.rkt")

(provide make-ExtPoint define-ExtPoint ExtPoint-equal?)

(define (make-ExtPoint name join empty)
  (ExtPoint name (gensym name) (Monoid join empty)))

(define-syntax (define-ExtPoint stx)
  (with-syntax* ([(_ name join empty) stx]
                 [name-join  (format-id stx "~a-join" #'name)]
                 [name-empty (format-id stx "~a-empty" #'name)])
    #`(begin
        (define name (make-ExtPoint 'name join empty))
        (define name-join (ExtPoint-join name))
        (define name-empty (ExtPoint-empty name)))))

(define ExtPoint-equal?
  (lambda (x y) (eq? (ExtPoint-uid x) (ExtPoint-uid y)))
  ;(match-lambda** [((ExtPoint _ uid1 _) (ExtPoint _ uid2 _)) (eq? uid1 uid2)])
  )


;; Environments are immutable hashtables, mapping extension-points to their
;; monoid values. If an extension-point is absent, it is the same as being
;; mapped to its empty value.

(provide env-empty env-join2 env-join* env-join env-monoid env-single env-get)

;; hashtable type used for extension-point envs. I don't think this is
;; necessary, since racket *can* hash functions, and we never intend to generate
;; two ExtPoints with the same uid.
;;
;; (define-custom-hash-types ext-hash
;;   #:key? ExtPoint?
;;   ExtPoint-equal?
;;   (lambda (x) (eq-hash-code (ExtPoint-uid x)))
;;   (lambda (x) (equal-secondary-hash-code (ExtPoint-uid x))))

(define env-empty (hash))

(define (env-join2 a b)
  (hash-union a b (lambda (k x y) ((ExtPoint-join k) x y))))

(define (env-join* es) (reduce es env-empty env-join2))

(define (env-join . es) (env-join* es))

(define env-monoid (Monoid env-join2 env-empty))

(define (env-single ext-point value)
  (hash ext-point value))

(define (env-get ext-point ext)
  (hash-get ext-point ext
    (lambda () (ExtPoint-empty ext-point))))


;; Tools for defining interfaces on hashes and convenient ways to construct
;; hashes. We use hashes to represent records (and thus, more or less, OO-style
;; objects) in our language. Methods are represented by keys mapped to
;; functions; properties by keys mapped to values.

(provide define-accessors define-accessor define-form make-record)

(define-syntax (define-accessors stx)
  (syntax-parse stx
    [(_ prefix:id accessor ...)
      (let ([mk-name (lambda (name) (format-id stx "~a-~a" #'prefix name))])
        #`(begin
            #,@(for/list [(axor (syntax->list #'(accessor ...)))]
                 (syntax-parse axor
                   [(name:id param:id ...)
                     #`(define-accessor #,(mk-name #'name) param ...)
                     #`(define (#,(mk-name #'name) self param ...)
                         ((hash-get 'name self) param ...))]
                   [name:id
                     #`(define-accessor #,(mk-name #'name))]))))]))

(define-syntax (define-accessor stx)
  (syntax-parse stx
    [(_ name:id)
      #`(define (name self) (hash-get 'name self))]
    [(_ name:id param:id ...)
      #`(define (name self param ...) ((hash-get 'name self) param ...))]))

(define-syntax (define-form stx)
  (with-syntax* ([(_ form (field ...) method ...) stx]
                 )
    #`(define (form field ...)
        (make-record
          [type 'form]
          [field field] ...
          method ...))))

(define-syntax (make-record stx)
  (let* ([bindings (cdr (syntax->list stx))]
         [names (map (lambda (b) (syntax-parse b
                              [(name:id value) #'name]
                              [((name:id param:id ...) body ...) #'name]))
                  bindings)]
         [exprs (map (lambda (b) (syntax-parse b
                              [(name:id value) #'value]
                              [((name:id param:id ...) body ...)
                                #'(lambda (param ...) body ...)]))
                  bindings)])
    (with-syntax ([(name ...) names] [(expr ...) exprs])
      #`(make-immutable-hash
          (let* ((name expr) ...)
            `((name . ,name) ...))))))


;; -- Built-in ParseEnv extension points --
;; Convention: extension point names begin with "@"

(provide
  @decls @decls-join @decls-empty @decl-parser
  @exprs @exprs-join @exprs-empty @expr-parser
  @infixes @infixes-join @infixes-empty @infix-precedence @infix-parser
  @pats @pats-join @pats-empty @pat-parser)

;; Maps tokens to "@decl"s.
(define-ExtPoint @decls hash-union (hash))
(define-accessors @decl
  parser ;; Parser Decl (see "parts of speech" below for what Decl is)
  )

;; Maps tokens to (@prefix-expr)s
(define-ExtPoint @exprs hash-union (hash))
(define-accessors @expr
  parser     ;; Parser Expr
  )

;; Maps tokens to (@infix)es
(define-ExtPoint @infixes hash-union (hash))
(define-accessors @infix
  precedence ;; Int
  parser)    ;; Int, Expr -> Parser Expr

;; Maps tokens to (@pat)s
(define-ExtPoint @pats hash-union (hash))
(define-accessors @pat
  parser) ;; Parser Pat


;; -- Built-in ResolveEnv extension points --

;; TODO: should ResolveEnv really be represented as an env? or is it too
;; special-purpose? What legitimate extensions to ResolveEnv are possible?

(provide
  @vars @vars-join @vars-empty
  @var-style @var-id @var-tag-id @var-tag-arity
  )

;; maps var names to hashes of info about them.
;; hash keys:
;; - style: one of '(var ctor)
;; - id: the IR identifier for the value of this variable.
;;
;; Hash keys for ctors:
;; - tag-id: The IR id for the tag for this ctor.
;; - tag-arity: Arity of ctor.
(define-ExtPoint @vars hash-union (hash))

(define (@vars-var name id) (hash name (hash 'type 'var 'id id)))
(define (@vars-ctor name id tag-id tag-arity)
  (hash name (hash 'type 'ctor 'id id 'tag-id tag-id 'tag-arity tag-arity)))

(define-accessors @var
  style id tag-id tag-arity)


;; Builtin parts of speech.
;;
;; "Parts of speech" are interfaces for various parts of the language AST; e.g.
;; expressions, declarations, patterns.
;;
;; Parts of speech are defined by the interface they present so that people can
;; add new forms with new behavior. E.g. an Expr is anything that has a 'compile
;; "method" that takes a ResolveEnv and produces an IR expression.

(provide
  expr-compile
  decl-resolveExt decl-compile
  pat-resolveExt pat-compile
  result-resolveExt result-parseExt
  nodule-name nodule-resolveExt nodule-parseExt)

(define-accessors expr
  (compile renv))                       ; ResolveEnv -> IR

(define-accessors decl
  resolveExt                            ; ResolveEnv
  (compile renv))                       ; ResolveEnv -> [(Id, IR)]

(define-accessors pat
  resolveExt
  ;; Not sure of the best way to present this interface.
  ;; ResolveEnv, IR, IR, IR -> IR
  (compile renv subject on-success on-failure))

;; The "result" of parsing a top-level declaration. Not exactly a part of
;; speech, but acts like one.
(define-accessors result
  resolveExt
  parseExt)

(define-accessors nodule ;; can't use "module", it means something in Racket
  name
  resolveExt
  parseExt)


;; Builtin expressions forms
(provide expr:lit expr:var expr:call expr:seq expr:lambda expr:let)

(define-form expr:lit (value)
  [sexp value]
  [(compile env) (list 'quote value)])

(define-form expr:var (name)
  [sexp `(var ,name)]
  [(compile env)
    ;; TODO: better error handling
    (hash-get 'id
      (hash-get name (env-get @vars env)
        (lambda () (error 'expr:var "unbound variable ~v" name))))])

(define-form expr:call (func args)
  [(compile env) (map (lambda (x) (expr-compile x env)) (cons func args))])

(define-form expr:seq (a b)
  [(compile env) `(begin ,(expr-compile a env) ,(expr-compile b env))])

(define-form expr:lambda (params expr)
  ;; TODO: case-lambdas? patterns as parameters?
  ;; TODO: check for duplicate names in params
  [(compile env)
    (let* ([ids (map gensym params)]
           [vars (hash-from-keys-values
                   params
                   (map (lambda (id) (hash 'id id)) ids))]
           [inner-env (env-join env (env-single @vars vars))])
      `(lambda (,@ids)
         ,(expr-compile expr inner-env)))])

(define-form expr:let (decls exp)
  [(compile env)
    (let loop ([decls decls] [env env])
      (match decls
        ['() (expr-compile exp env)]
        [(cons d ds)
          `(letrec ,(decl-compile d env)
             ,(loop ds (env-join env (decl-resolveExt d))))]))])


;; Builtin pattern forms
(provide pat:one pat:zero pat:let pat:var pat:ann pat:vector)

(define-form pat:one () ;; "underscore" pattern, _, succeeds binding nothing
  [resolveExt env-empty]
  [(compile env subject on-success on-failure) on-success])

(define-form pat:zero (names) ;; pattern that always fails, binding names
  [ids (map gensym names)]
  [resolveExt (env-single @vars (hash-from-keys-values names
                                  (map (lambda (x) (hash 'id x)) ids)))]
  [(compile env subject on-success on-failure) on-failure])

(define-form pat:let (name expr) ;; always binds name to expr
  [id (gensym name)]
  [resolveExt (env-single @vars (hash name (hash 'id id)))]
  [(compile env subject on-success on-failure)
    `(let ((,id ,(expr-compile expr env))) ,on-success)])

(define-form pat:var (name)
  [id (gensym name)]
  [resolveExt (env-single @vars (hash name (hash 'id id)))]
  [(compile env subject on-success on-failure)
    `(let ([,id ,subject]) ,on-success)])

(define-form pat:ann (name args)
  [args-pat (pat:vector args)]
  [resolveExt (pat-resolveExt args-pat)]
  [(compile env subject on-success on-failure)
    ;; TODO: useful error handling.
    (let* ([info (hash-get name (env-get @vars env)
                   (lambda () (error 'pat:ann "unbound ctor ~v" name)))]
           [ctor-id (hash-get 'id info)]
           [tag-id (hash-get 'tag-id info
                     (lambda () (error 'pat:ann "~v is not a ctor" name)))]
           [tag-arity (hash-get 'tag-arity info)]
           [tmp (gensym 'tmp)])
      `(if (ann-isa? ,tag-id ,subject)
         ;; match each pat in args against (ann-args subject)
         (let ((,tmp (ann-args ,subject)))
           ,(pat-compile args-pat env tmp on-success on-failure))
         ,on-failure))])

(define-form pat:vector (elems)
  [resolveExt (env-join* (map pat-resolveExt elems))]
  [(compile env subject on-success on-failure)
    (let loop ([i 0] [env env])
      (if (>= i (vector-length elems)) on-success
        (let ([tmp (gensym 'tmp)]
              [elem (vector-ref elems i)])
          `(let ([,tmp (vector-ref ,subject ',i)])
             ,(pat-compile elem tmp
                (loop (+ i 1) (env-join env (pat-resolveExt elem)))
                on-failure)))))])

;; TODO: pat:and, pat:or, pat:lit, pat:guard


;; Builtin declaration forms
(provide decl:val decl:fun decl:rec decl:tag)

;; (decl:val Symbol Expr)
(define-form decl:val (name expr)
  [id (gensym name)]
  [resolveExt (env-single @vars (hash name (hash 'id id)))]
  [(compile env) `((,id ,(expr-compile expr env)))])

;; (decl:fun Symbol [Symbol] Expr)
(define-form decl:fun (name params expr)
  ;; TODO: functions with branches
  [id (gensym name)]
  [resolveExt (env-single @vars (hash name (hash 'id id)))]
  [(compile env)
    (let ([inner-env (env-join env resolveExt)])
      `((,id ,(expr-compile (expr:lambda params expr) inner-env))))])

;; (decl:rec [Decl])
(define-form decl:rec (decls)
  [resolveExt (env-join (map decl-resolveExt decls))]
  [(compile env)
    (let ([env (env-join env resolveExt)])
      (apply append (map (lambda (x) (decl-compile x env)) decls)))])

;; (decl:tag Symbol [Symbol])
(define-form decl:tag (name params)
  [id (gensym (format "ctor:~a" name))]
  [tag-id (gensym (format "tag:~a" name))]
  [info (hash 'id id 'tag-id tag-id 'tag-arity (length params))]
  [resolveExt (env-single @vars (hash name info))]
  [(compile env)
    `((,tag-id (new-tag name params))
      (,id (lambda (,@params) (make-ann ,tag-id ,@params))))])


;; Builtin result forms
(provide result:decl result:import)

;; (result:decl Decl)
(define-form result:decl (decl)
  [resolveExt (decl-resolveExt decl)]
  [parseExt env-empty])

;; TODO: more powerful imports (qualifying, renaming, etc.)
;; (result:import Nodule)
(define-form result:import (nodule)
  [resolveExt (nodule-resolveExt nodule)]
  [parseExt (nodule-parseExt nodule)])

;; (define-form result:define-extension-point (name join empty)
;;   [ext-point (ExtPoint name (gensym name) (Monoid join empty))]
;;   [resolveExt env-empty]
;;   [parseExt env-empty])

;; ;; TODO
;; (define-decl decl:define-extension-point (name oper unit))
;; (define-decl decl:define-extension (point expr))

;; ;; (decl:module Symbol [Decl])
;; (define-decl decl:module (name body)
;;   [id (gensym name)]
;;   [info (hash 'id id)]
;;   )

;; ;; (decl:import Symbol Module)
;; ;; where Module is a hash with (parseExt resolveExt) keys.
;; ;; the Module info gets provided by the parsers.
;; (define-decl decl:import (name nodule)
;;   )

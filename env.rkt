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


;; Built-in extension points
(provide
  @decls @decls-join @decls-empty
  @exprs @exprs-join @exprs-empty
  @pats @pats-join @pats-empty)

;; Convention: extension point names begin with "@"
;; -- ParseEnv extension points --

;; Maps tokens to (Parser decl)s (see parts of speech, below)
(define-ExtPoint @decls hash-union (hash))
;; Maps tokens to (Parser expr)s
(define-ExtPoint @exprs hash-union (hash))
;; Maps tokens to (Parser pat)s
(define-ExtPoint @pats hash-union (hash))


;; -- ResolveEnv extension points --

;; TODO: should ResolveEnv really be represented as an env? or is it too
;; special-purpose? What legitimate extensions to ResolveEnv are possible? Do
;; extensions need to keep in mind the difference between compile-time and
;; run-time bindings, the way @vars does?

(provide
  env-mark-compile-time env-mask
  @vars @vars-join @vars-empty)

;; maps var names to hashes of info about them.
;; hash keys:
;; - type: one of '(var ctor mask)
;; - id: the IR identifier for the value of this variable.
;; - compile-time: #t if the value of this identifier is available at
;; compile-time.
;;
;; "Masks" indicate a variable is lexically bound, but cannot be used, because
;; the thing being compiled in this ResolveEnv will be run at compile-time and
;; the variable's value won't be available until run-time. Masks do not have a
;; 'id key. Hash keys for masks:
;; - info: The info-hash for the masked binding.
;;
;; Hash keys for ctors:
;; - tag-id: The IR id for the tag for this ctor.
;; - tag-arity: Arity of ctor.
(define-ExtPoint @vars hash-union (hash))

(define (@vars-var name id) (hash name (hash 'type 'var 'id id)))
(define (@vars-ctor name id tag-id tag-arity)
  (hash name (hash 'type 'ctor 'id id 'tag-id tag-id 'tag-arity tag-arity)))

(define (env-mark-compile-time env)
  (hash-put env
    (hash-map (env-get @vars env)
      (lambda (var info) (hash-put 'compile-time #t info)))
    env))

(define (env-mask env)
  (hash-put env
    (hash-map (env-get @vars env)
      (lambda (var info)
        (if (hash-get 'compile-time info (lambda () #f))
          info
          (hash 'type 'mask 'info info))))
    env))

;; Do I need this?
; (define (env-unmask env) )


;; "Parts of speech": interfaces for various parts of the language AST.
;; E.g. expressions, declarations, patterns
;;
;; Parts of speech are defined by the interface they present so that people can
;; add new forms with new behavior. E.g. an Expr is anything that has a 'compile
;; "method" that takes a ResolveEnv and produces an IR expression.
;;
;; Values of parts of speech (actual expr, decl, pattern nodes in the AST) are
;; represented by hashes mapping "methods" to their values. So the following is
;; just a convenience to make working with them in Racket easier.

(define-syntax (define-pos stx)
  (with-syntax* ([(_ pos methods ...) stx]
                 [define-pos_ (format-id stx "define-~a" #'pos)])
    #`(begin
        #,@(for/list ([method (syntax->list #'(methods ...))])
             (syntax-parse method
               [(name:id params:id ...)
                 #`(define #,(format-id stx "~a-~a" #'pos #'name)
                     (lambda (self params ...)
                       ;; TODO: error message on hash-get failure?
                       ((hash-get 'name self) params ...)))]
               [name:id
                 #`(define #,(format-id stx "~a-~a" #'pos #'name)
                     (lambda (self) (hash-get 'name self)))]))
        (define-syntax-rule
          (define-pos_ name (field (... ...)) method (... ...))
          (define-form pos name (field (... ...)) method (... ...))))))

(define-syntax (define-form stx)
  (with-syntax* ([(_ pos form (field ...) method ...) stx]
                 )
    #`(define (form field ...)
        (hash-put 'type 'form
          (magical-hash
            [field field] ...
            method ...)))))

(define-syntax (magical-hash stx)
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


;;; Builtin parts of speech and forms
(define-pos decl
  parseExt                              ; ParseEnv
  resolveExt                            ; ResolveEnv
  (compile renv))                       ; ResolveEnv -> [(Id, IR)]

(define-pos expr
  (compile renv))                       ; ResolveEnv -> IR

(define-pos pat
  resolveExt
  ;; Not sure of the best way to present this interface.
  ;; ResolveEnv, IR, IR, IR -> IR
  (compile renv subject on-success on-failure))


;; Expressions
(define-expr expr:lit (value)
  [(compile env) (list 'quote value)])

(define-expr expr:var (name)
  [(compile env)
    ;; TODO: better error handling
    (hash-get 'id
      (hash-get name (env-get @vars env)
        (lambda () (error 'expr:var "unbound variable ~v" name))))])

(define-expr expr:call (func args)
  [(compile env) (map (lambda (x) (expr-compile x env)) (cons func args))])

(define-expr expr:seq (a b)
  [(compile env) `(begin ,(expr-compile a env) ,(expr-compile b env))])

(define-expr expr:lambda (params expr)
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

(define-expr expr:let (decls exp)
  [(compile env)
    (let loop ([decls decls] [env env])
      (match decls
        ['() (expr-compile exp env)]
        [(cons d ds)
          `(letrec ,(decl-compile d env)
             ,(loop ds (env-join env (decl-resolveExt d))))]))])


;; Declarations
;; (decl:val Symbol Expr)
(define-decl decl:val (name expr)
  [id (gensym name)]
  [parseExt env-empty]
  [resolveExt (env-single @vars (hash name (hash 'id id)))]
  [(compile env) `((,id ,(expr-compile expr env)))])

;; (decl:fun Symbol [Symbol] Expr)
(define-decl decl:fun (name params expr)
  ;; TODO: functions with branches
  [id (gensym name)]
  [parseExt env-empty]
  [resolveExt (env-single @vars (hash name (hash 'id id)))]
  [(compile env)
    (let ([inner-env (env-join env resolveExt)])
      `((,id ,(expr-compile (expr:lambda params expr) inner-env))))])

;; (decl:rec [Decl])
(define-decl decl:rec (decls)
  [parseExt (env-join (map decl-parseExt decls))]
  [resolveExt (env-join (map decl-resolveExt decls))]
  [(compile env)
    (let ([env (env-join env resolveExt)])
      (apply append (map (lambda (x) (decl-compile x env)) decls)))])

;; (decl:tag Symbol [Symbol])
(define-decl decl:tag (name params)
  [id (gensym (format "ctor:~a" name))]
  [tag-id (gensym (format "tag:~a" name))]
  [info (hash 'id id 'tag-id tag-id 'tag-arity (length params))]
  [parseExt env-empty]
  [resolveExt (env-single @vars (hash name info))]
  [(compile env)
    `((,tag-id (new-tag name params))
      (,id (lambda (,@params) (make-ann ,tag-id ,@params))))])

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


;; Patterns

(define-pat pat:one () ;; "underscore" pattern, _, succeeds binding nothing
  [resolveExt env-empty]
  [(compile env subject on-success on-failure) on-success])

(define-pat pat:zero (names) ;; pattern that always fails, binding names
  [ids (map gensym names)]
  [resolveExt (env-single @vars (hash-from-keys-values names
                                  (map (lambda (x) (hash 'id x)) ids)))]
  [(compile env subject on-success on-failure) on-failure])

(define-pat pat:let (name expr) ;; always binds name to expr
  [id (gensym name)]
  [resolveExt (env-single @vars (hash name (hash 'id id)))]
  [(compile env subject on-success on-failure)
    `(let ((,id ,(expr-compile expr env))) ,on-success)])

(define-pat pat:var (name)
  [id (gensym name)]
  [resolveExt (env-single @vars (hash name (hash 'id id)))]
  [(compile env subject on-success on-failure)
    `(let ([,id ,subject]) ,on-success)])

(define-pat pat:ann (name args)
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

(define-pat pat:vector (elems)
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

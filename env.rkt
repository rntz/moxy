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

(provide define-accessors define-accessor define-form record)

(define-syntax (define-accessors stx)
  (syntax-parse stx
    [(_ prefix:id accessor ...)
      (let ([mk-name (lambda (name) (format-id stx "~a-~a" #'prefix name))])
        #`(begin
            #,@(for/list [(axor (syntax->list #'(accessor ...)))]
                 (syntax-parse axor
                   [(name:id param:id ...)
                     #`(define-accessor #,(mk-name #'name) name (param ...))]
                   [name:id
                     #`(define-accessor #,(mk-name #'name) name)]))))]))

(define-syntax (define-accessor stx)
  (syntax-parse stx
    [(_ axor-name:id key:id)
      #`(define (axor-name self [or-else (lambda () (error 'axor-name
                                                 "absent field: ~v" 'key))])
          (hash-get 'key self or-else))]
    [(_ axor-name:id key:id (param:id ...))
      #`(define (axor-name self param ...)
          ((hash-get 'key self (lambda ()
                                 (error 'axor-name "absent method: ~v" 'key)))
            param ...))]))

(define-syntax (define-form stx)
  (with-syntax* ([(_ form (field ...) method ...) stx]
                 )
    #`(define (form field ...)
        (record
          [type 'form]
          [field field] ...
          method ...))))

(define-syntax (record stx)
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
  @exprs @exprs-join @exprs-empty @expr-parser
  @infixes @infixes-join @infixes-empty @infix-precedence @infix-parser
  @pats @pats-join @pats-empty @pat-parser
  @decls @decls-join @decls-empty @decl-parser
  @tops @tops-join @tops-empty @top-parse-eval)

;; Maps tokens to (@expr)s
(define-ExtPoint @exprs hash-union (hash))
(define-accessors @expr
  parser     ;; Parser Expr
  )

;; Maps tokens to (@infix)es
(define-ExtPoint @infixes hash-union (hash))
(define-accessors @infix
  precedence               ;; Int
  ;; parser : Expr -> Parser Expr
  ;; takes the "left argument" to the infix operator.
  ;; needs to parse the right argument(s) itself.
  ;; so this really allows any non-prefix operator, not just infix
  ;; (postfix or ternary operators, for example)
  (parser left-expr))

;; Maps tokens to (@pat)s
(define-ExtPoint @pats hash-union (hash))
(define-accessors @pat
  parser) ;; Parser Pat

;; Maps tokens to (@decl)s.
(define-ExtPoint @decls hash-union (hash))
(define-accessors @decl
  parser ;; Parser Decl (see "parts of speech" below for what Decl is)
  )

;; Maps tokens to (@top)s.
(define-ExtPoint @tops hash-union (hash))
(define-accessors @top
  ;; ResolveEnv -> Parser Result
  ;; TODO: Do we need to pass in some kind of "state" for the evaluator?
  (parse-eval resolve-env))


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

(define-form @var:var (name id) [style 'var])
(define-form @var:ctor (name id tag-id tag-arity) [style 'ctor])

(define (@vars-var name id) (hash name (@var:var name id)))
(define (@vars-ctor name id tag-id tag-arity)
  (hash name (@var:ctor name id tag-id tag-arity)))

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
  expr-compile expr-sexp
  decl-resolveExt decl-compile decl-sexp
  pat-resolveExt pat-compile pat-sexp
  result-resolveExt result-parseExt
  nodule-name nodule-resolveExt nodule-parseExt)

(define-accessors expr
  (sexp)
  (compile resolve-env))                ; ResolveEnv -> IR

(define-accessors decl
  (sexp)
  resolveExt                            ; ResolveEnv
  (compile resolve-env))                ; ResolveEnv -> [(Id, IR)]

(define-accessors pat
  (sexp)
  resolveExt
  ;; Not sure of the best way to present this interface.
  ;; ResolveEnv, IR, IR, IR -> IR
  (compile resolve-env subject on-success on-failure))

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
(provide expr:lit expr:var expr:call expr:seq expr:lambda expr:let expr:if)

(define-form expr:lit (value)
  [(sexp) (if (or (string? value) (number? value)) value (list 'quote value))]
  [(compile env) (list 'quote value)])

(define-form expr:var (name)
  [(sexp) name]
  [(compile env)
    ;; TODO: better error handling
    (hash-get 'id
      (hash-get name (env-get @vars env)
        (lambda () (error 'expr:var "unbound variable ~v" name))))])

(define-form expr:call (func args)
  [(sexp) (map expr-sexp (cons func args))]
  [(compile env) (map (lambda (x) (expr-compile x env)) (cons func args))])

(define-form expr:seq (a b)
  [(sexp) `(begin ,(expr-sexp a) ,(expr-sexp b))]
  [(compile env) `(begin ,(expr-compile a env) ,(expr-compile b env))])

(define-form expr:lambda (params expr)
  ;; TODO: case-lambdas? patterns as parameters?
  ;; TODO: check for duplicate names in params
  [(sexp) `(lambda ,params ,(expr-sexp expr))]
  [(compile env)
    (let* ([ids (map gensym params)]
           [vars (hash-from-keys-values
                   params
                   (map @var:var params ids))]
           [inner-env (env-join env (env-single @vars vars))])
      `(lambda (,@ids)
         ,(expr-compile expr inner-env)))])

(define-form expr:let (decls exp)
  [(sexp) `(let ,(map decl-sexp decls) ,(expr-sexp exp))]
  [(compile env)
    (let loop ([decls decls] [env env])
      (match decls
        ['() (expr-compile exp env)]
        [(cons d ds)
          `(letrec ,(decl-compile d env)
             ,(loop ds (env-join env (decl-resolveExt d))))]))])

(define-form expr:if (subject then else)
  [(sexp) `(if ,(expr-sexp subject) ,(expr-sexp then) ,(expr-sexp else))]
  [(compile env)
    `(if ,(expr-compile subject env)
       ,(expr-compile then env)
       ,(expr-compile else env))])


;; Builtin pattern forms
;; TODO: sexp method
(provide pat:one pat:zero pat:let pat:var pat:ann pat:vector)

(define-form pat:one () ;; "underscore" pattern, _, succeeds binding nothing
  [resolveExt env-empty]
  [(compile env subject on-success on-failure) on-success])

(define-form pat:zero (names) ;; pattern that always fails, binding names
  [ids (map gensym names)]
  [resolveExt (env-single @vars (hash-from-keys-values names
                                  (map @var:var names ids)))]
  [(compile env subject on-success on-failure) on-failure])

(define-form pat:let (name expr) ;; always binds name to expr
  [id (gensym name)]
  [resolveExt (env-single @vars (@vars-var name id))]
  [(compile env subject on-success on-failure)
    `(let ((,id ,(expr-compile expr env))) ,on-success)])

(define-form pat:var (name)
  [id (gensym name)]
  [resolveExt (env-single @vars (@vars-var name id))]
  [(compile env subject on-success on-failure)
    `(let ([,id ,subject]) ,on-success)])

(define-form pat:ann (name args)
  [args-pat (pat:vector args)]
  [resolveExt (pat-resolveExt args-pat)]
  [(compile env subject on-success on-failure)
    ;; TODO: useful error handling.
    (let* ([info (hash-get name (env-get @vars env)
                   (lambda () (error 'pat:ann "unbound ctor ~v" name)))]
           [ctor-id (@var-id info)]
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
;; TODO: sexp method
(provide decl:val decl:fun decl:rec decl:tag)

;; (decl:val Symbol Expr)
(define-form decl:val (name expr)
  [(sexp) `(val ,name ,(expr-sexp expr))]
  [id (gensym name)]
  [resolveExt (env-single @vars (@vars-var name id))]
  [(compile env) `((,id ,(expr-compile expr env)))])

;; (decl:fun Symbol [Symbol] Expr)
(define-form decl:fun (name params expr)
  [(sexp) `(fun ,name ,params ,(expr-sexp expr))]
  ;; TODO: functions with branches
  [id (gensym name)]
  [resolveExt (env-single @vars (@vars-var name id))]
  [(compile env)
    (let ([inner-env (env-join env resolveExt)])
      `((,id ,(expr-compile (expr:lambda params expr) inner-env))))])

;; (decl:rec [Decl])
(define-form decl:rec (decls)
  [(sexp) `(rec ,@(map decl-sexp decls))]
  [resolveExt (env-join (map decl-resolveExt decls))]
  [(compile env)
    (let ([env (env-join env resolveExt)])
      (apply append (map (lambda (x) (decl-compile x env)) decls)))])

;; (decl:tag Symbol [Symbol])
(define-form decl:tag (name params)
  [(sexp) `(tag ,name ,params)]
  [id (gensym (format "ctor:~a" name))]
  [tag-id (gensym (format "tag:~a" name))]
  [info (@var:ctor name id tag-id (length params))]
  [resolveExt (env-single @vars (hash name info))]
  [(compile env)
    `((,tag-id (new-tag name params))
      (,id (lambda (,@params) (make-ann ,tag-id ,@params))))])


;; Builtin result forms
(provide result:empty result:decl result:import)

(define-form result:empty ()
  [resolveExt env-empty]
  [parseExt env-empty])

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

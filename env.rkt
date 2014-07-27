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

(define (env-get ext-point env)
  (hash-get ext-point env
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
                              ;; TODO: must have at least one body expr
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
  ;; TODO: rename to @infix-exprs
  @infixes @infixes-join @infixes-empty @infix-precedence @infix-parser
  @pats @pats-join @pats-empty @pat-parser
  ;; TODO: unify @infix-exprs & @infix-pats somehow?
  @infix-pats @infix-pats-join @infix-pats-empty
  @infix-pat-precedence @infix-pat-parser
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

(define-ExtPoint @infix-pats hash-union (hash))
(define-accessors @infix-pat
  precedence               ;; Int
  ;; parser : Expr -> Parser Expr, see @infix-parser above for explanation
  (parser left-pat))

;; Maps tokens to (@decl)s.
(define-ExtPoint @decls hash-union (hash))
(define-accessors @decl
  parser ;; Parser Decl (see "parts of speech" below for what Decl is)
  )

;; Maps tokens to (@top)s.
(define-ExtPoint @tops hash-union (hash))
(define-accessors @top
  ;; ResolveEnv, NS -> Parser Result
  ;; ns is the Racket namespace in which we eval code.
  (parse-eval resolve-env ns))


;; -- Built-in ResolveEnv extension points --

;; TODO: should ResolveEnv really be represented as an env? or is it too
;; special-purpose? What legitimate extensions to ResolveEnv are possible?

(provide
  @vars @vars-join @vars-empty
  @var:var @var:ctor @vars-var @vars-ctor
  @var-style @var-id @var-tag-id @var-tag-params
  )

;; maps var names to hashes of info about them.
;; hash keys:
;; - style: one of '(var ctor)
;; - id: the IR identifier for the value of this variable.
;;
;; Hash keys for ctors:
;; - tag-id: The IR id for the tag for this ctor.
;; - tag-params: (Maybe [Symbol]). The parameters for the ctor, if any.
(define-ExtPoint @vars hash-union (hash))

;; TODO: should this go here or in parse-builtins.rkt?
(define-form @var:var (name id) [style 'var])
(define-form @var:ctor (name id tag-id tag-params) [style 'ctor])

(define (@vars-var name id) (hash name (@var:var name id)))
(define (@vars-ctor name id tag-id tag-params)
  (hash name (@var:ctor name id tag-id tag-params)))

(define-accessors @var
  style id tag-id tag-params)


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

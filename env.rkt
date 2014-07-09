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

(provide env-empty env-join2 env-join env-monoid env-single env-get)

;; hashtable type used for extension-point envs.
;;
;; I don't think this is strictly necessary, since racket *can* hash functions,
;; and we never intend to generate two ExtPoints with the same uid.
(define-custom-hash-types ext-hash
  #:key? ExtPoint?
  ExtPoint-equal?
  (lambda (x) (eq-hash-code (ExtPoint-uid x)))
  (lambda (x) (equal-secondary-hash-code (ExtPoint-uid x))))

(define env-empty (make-immutable-ext-hash))

(define (env-join2 a b)
  (hash-union a b (lambda (k x y) ((ExtPoint-join k) x y))))

(define (env-join . es)
  (reduce es env-empty env-join2))

(define env-monoid (Monoid env-join2 env-empty))

(define (env-single ext-point value)
  (make-immutable-ext-hash `((,ext-point . ,value))))

(define (env-get ext ext-point)
  (hash-get-or-else ext-point ext
    (lambda () (ExtPoint-empty ext-point))))


;; Built-in extension points
(provide
  @decls @decls-join @decls-empty
  @vars @vars-join @vars-empty)

;; Convention: extension point names begin with "@"
;; -- ParseEnv extension points --

;; Maps tokens to (Parser decl)s (see parts of speech, below)
(define-ExtPoint @decls hash-union (hash))
;; Maps tokens to (Parser expr)s
(define-ExtPoint @exprs hash-union (hash))
;; Maps tokens to (Parser pat)s
(define-ExtPoint @pats hash-union (hash))

;; -- ResolveEnv extension points --

;; maps var names to hashes of info about them.
;; hash keys:
;; - id: the IR identifier for the value of this variable.
;;
;; Hash keys present for tags/ctors only:
;; - tag-id: The IR id for the tag for this ctor.
;; - tag-arity: Arity of ctor.
(define-ExtPoint @vars hash-union (hash))


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
        (magical-hash
          [field field] ...
          method ...))))

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
          (letrec ((name expr) ...)
            `((name . ,name) ...))))))

;;; ---- THE CODE I WANT TO WRITE ----
;; defines: define-decl, decl-{parse-ext,resolve-ext,compile}
(define-pos decl parse-ext resolve-ext (compile renv))
(define-pos expr (compile renv))

;; defines (lit value)
(define-expr lit (value)
  [(compile env) (list 'quote value)])

(define-decl val (var expr)
  [id (gensym var)]
  [parse-ext env-empty]
  [resolve-ext
    (env-single @vars (hash var (hash 'id id)))]
  [(compile env)
    `((,id ,(expr-compile expr env)))])

;;; ----- END CODE I WANT TO WRITE -----

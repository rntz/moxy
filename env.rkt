#lang racket

(require (for-syntax racket/syntax))    ;format-id

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

(provide env-empty env-join2 env-join env-monoid env-make env-get)

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

(define (env-make ext-point value)
  (make-immutable-ext-hash `((,ext-point . ,value))))

(define (env-get ext ext-point)
  (hash-get-or-else ext-point ext
    (lambda () (ExtPoint-empty ext-point))))


;; Built-in extension points
(provide
  @decls @decls-join @decls-empty
  @vars @vars-join @vars-empty)

;; Convention: extension point names begin with "@"
;; for ParseEnv:
;; Maps tokens to (Parser Decl)s (see parts of speech, below)
(define-ExtPoint @decls hash-union (hash))

;; for ResolveEnv:
;; variable scoping point
;; maps variables to hashes of info about them.
;; hash keys:
;; - id: the IR identifier to compile this var to.
(define-ExtPoint @vars hash-union (hash))


;; "Parts of speech": interfaces for various parts of the language AST.
;; E.g. expressions, declarations, patterns
;;
;; These are defined by the interface they present so that people can add new
;; forms with new behavior.

;; (define-syntax define-methods-prefixed
;;   (syntax-rules ()
;;     [(define-methods-prefixed iface prefix) (begin)]
;;     [(define-methods-prefixed iface prefix name names ...)
;;       (begin
;;         (define-method-prefixed iface prefix name)
;;         (define-methods-prefixed iface prefix names ...))]))

;; (define-syntax (define-form stx)
;;   (with-syntax* ([(_ pos form (fields ...) body ...) stx]
;;                   [pos<%>    (format-id stx "~a<%>" #'pos)]
;;                   [pos:form% (format-id stx "~a:~a%" #'pos #'form)]
;;                   [pos:form  (format-id stx "~a:~a" #'pos #'form)])
;;     (let ([fields (syntax->list #'(fields ...))])
;;       #`(begin
;;           (define pos:form%
;;             (class* object% (pos<%>)
;;               #,@(for/list ([field fields]) #`(init-field #,field))
;;               (super-new)
;;               body ...))
;;           (define (pos:form #,@fields)
;;             (new pos:form% #,@(for/list ([field fields]) #`[#,field #,field])))
;;           ))))

;; (define-syntax (define-pos stx)
;;   (with-syntax* ([(_ pos (methods ...)) stx]
;;                   [pos<%>     (format-id stx "~a<%>" #'pos)]
;;                   [define-pos_ (format-id stx "define-~a" #'pos)])
;;     #`(begin
;;         (define pos<%> (interface () methods ...))
;;         #,@(for/list ([method (syntax->list #'(methods ...))])
;;              (with-syntax ([method method])
;;                ;; e.g. (define decl-compile ...)
;;                #`(define #,(format-id stx "~a-~a" #'pos #'method)
;;                    (let ([g (generic pos<%> method)])
;;                      (lambda (object . args) (send-generic object g . args))))))
;;         (define-syntax-rule (define-pos_ form (fields (... ...)) body (... ...))
;;           (define-form pos form (fields (... ...)) body (... ...))))))

;; ;;; ---- THE CODE I WANT TO WRITE ----
;; ;; defines interface pos:decl<%>
;; ;; PROBLEM: parse-ext, resolve-ext, compile become specific to decl<%>
;; ;; defines macro define-decl

;; ;; defines: define-Decl, Decl, Decl-{parse-ext,resolve-ext,compile}
;; (define-pos Decl (parse-ext resolve-ext compile))

;; (define-pos expr (compile))

;; (define-expr lit (value) (define/public (compile env) (list 'quote value)))

;; ;; defines class decl:val%
;; ;; defines constructor decl:val
;; (define-decl val (var expr)
;;   (define id (gensym var))
;;   (define/public (parse-ext) env-empty)
;;   (define/public (resolve-ext)
;;     (env-make @vars (hash var (hash 'id id))))
;;   (define/public (compile env)
;;     `((,id ,(expr-compile expr env)))))

;;; ----- END CODE I WANT TO WRITE -----

(displayln "env.rkt loaded")

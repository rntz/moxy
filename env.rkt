#lang racket

(require (for-syntax racket/syntax))    ;format-id

(require "util.rkt")

(provide
  extension-point make-extension-point
  extension-point-name extension-point-empty extension-point-join
  define-extension-point
  env-empty env-join env-make env-get)

(struct extension-point (uid name empty join)
  #:constructor-name make-extension-point-internal
  #:methods
  gen:equal+hash
  [(define (equal-proc a b equal?-recur)
     (eq? (extension-point-uid a) (extension-point-uid b)))
   (define (hash-proc a hash-recur)
     (hash-recur (extension-point-uid a)))
    (define (hash2-proc a hash-recur)
      (hash-recur (extension-point-uid a)))])

(define (make-extension-point name empty join)
  (make-extension-point-internal (gensym name) name empty join))

(define-syntax-rule (define-extension-point name empty join)
  (define name (make-extension-point (quote name) empty join)))


;; Environments are immutable hashtables, mapping extension-points to their
;; monoid values. If an extension-point is absent, it is the same as being

;; mapped to its empty value.
(define env-empty (hash))

(define (env-join . es)
  (hash-unions es extension-point-join))

(define (env-make ext-point value)
  (hash ext-point value))

(define (env-get ext ext-point)
  (dict-ref ext ext-point (extension-point-empty ext-point)))


;; Built-in extension points
;; Convention: extension point names begin with "@"
;; for ParseEnv:
;; Maps tokens to (Parser Decl)s (see parts of speech, below)
(define-extension-point @decls (hash) dict-union)

;; for ResolveEnv:
;; variable scoping point
;; maps variables to hashes of info about them.
;; hash keys:
;; - id: the IR identifier to compile this var to.
(define-extension-point @vars (hash) dict-union)

(displayln "points.rkt loaded")


;; "Parts of speech": interfaces for various parts of the language AST.
;; E.g. expressions, declarations, patterns
;;
;; These are defined by the interface they present so that people can add new
;; forms with new behavior.

(define-syntax define-methods-prefixed
  (syntax-rules ()
    [(define-methods-prefixed iface prefix) (begin)]
    [(define-methods-prefixed iface prefix name names ...)
      (begin
        (define-method-prefixed iface prefix name)
        (define-methods-prefixed iface prefix names ...))]))

(define-syntax (define-form stx)
  (with-syntax* ([(_ pos form (fields ...) body ...) stx]
                  [pos<%>    (format-id stx "~a<%>" #'pos)]
                  [pos:form% (format-id stx "~a:~a%" #'pos #'form)]
                  [pos:form  (format-id stx "~a:~a" #'pos #'form)])
    (let ([fields (syntax->list #'(fields ...))])
      #`(begin
          (define pos:form%
            (class* object% (pos<%>)
              #,@(for/list ([field fields]) #`(init-field #,field))
              (super-new)
              body ...))
          (define (pos:form #,@fields)
            (new pos:form% #,@(for/list ([field fields]) #`[#,field #,field])))
          ))))

(define-syntax (define-pos stx)
  (with-syntax* ([(_ pos (methods ...)) stx]
                  [pos<%>     (format-id stx "~a<%>" #'pos)]
                  [define-pos_ (format-id stx "define-~a" #'pos)])
    #`(begin
        (define pos<%> (interface () methods ...))
        #,@(for/list ([method (syntax->list #'(methods ...))])
             (with-syntax ([method method])
               ;; e.g. (define decl-compile ...)
               #`(define #,(format-id stx "~a-~a" #'pos #'method)
                   (let ([g (generic pos<%> method)])
                     (lambda (object . args) (send-generic object g . args))))))
        (define-syntax-rule (define-pos_ form (fields (... ...)) body (... ...))
          (define-form pos form (fields (... ...)) body (... ...))))))

;;; ---- THE CODE I WANT TO WRITE ----
;; defines interface pos:decl<%>
;; PROBLEM: parse-ext, resolve-ext, compile become specific to decl<%>
;; defines macro define-decl
(define-pos decl (parse-ext resolve-ext compile))
(define-pos expr (compile))

(define-expr lit (value) (define/public (compile env) (list 'quote value)))

;; defines class decl:val%
;; defines constructor decl:val
(define-decl val (var expr)
  (define id (gensym var))
  (define/public (parse-ext) env-empty)
  (define/public (resolve-ext)
    (env-make @vars (hash var (hash 'id id))))
  (define/public (compile env)
    `((,id ,(expr-compile expr env)))))

;;; ----- END CODE I WANT TO WRITE -----

(displayln "env.rkt loaded")

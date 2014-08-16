#lang racket

(require
  (for-syntax
    (only-in racket/set list->set set-member?)
    syntax/parse
    (only-in racket/syntax format-id with-syntax*)))

(require "util.rkt")
(require "values.rkt")

;; A simple object system. Objects are immutable hashtables; everything in this
;; file is a mere convenience around creating & manipulating such hashes.
;;
;; This transparency is important so that we can share objects between Racket
;; and the implemented language without any Racket-specific magic "showing
;; through" in the implemented language.
;;
;; Properties are represented by symbols mapped to the properties' value.
;; Methods are represented by symbols mapped to a function that runs that
;; method. The function does *not* take the object itself as an argument; if it
;; needs to refer to itself, it must close over itself.
;;
;; There is no inheritance, and indeed no classes per se. "Forms", our
;; equivalent of classes, are just a convenience for constructing similar
;; objects.
;;
;; TODO: explain interfaces & accessors


(provide define-iface define-accessors define-accessor define-form record)

(define-for-syntax (make-define-form stx form fields ifaces methods)
  (with-syntax ([form form]
                [(field ...) fields]
                [(iface ...) ifaces]
                [(method ...) methods])
    (define (method-name a)
        (if (pair? a) (car a) a))
      (define fields (syntax->datum #'(field ...)))
      (define methods (map (compose method-name car)
                        (syntax->datum #'(method ...))))
      (define all-methods (list->set (append fields methods)))
      (for* ([ifc (syntax->list #'(iface ...))]
             [m (map method-name (syntax-local-value ifc))])
        (unless (set-member? all-methods m)
          (raise-syntax-error #f (format "method ~v not implemented" m) stx)))
      #`(define (form field ...)
          (record
            [#,'form 'form]
            [field field] ...
            method ...))))

(define-syntax (define-iface stx)
  (syntax-parse stx
    [(_ iface-name accessor ...)
      #`(begin
          (define-syntax iface-name '(accessor ...))
          (define-accessors iface-name accessor ...)
          (define-syntax (#,(format-id #'iface-name "define-~a" #'iface-name)
                           stx)
            (syntax-parse stx
              [(_ form-name:id (field:id (... ...)) method (... ...))
                (make-define-form stx
                  (format-id #'form-name "~a:~a" #'iface-name #'form-name)
                  #'(field (... ...))
                  #'(iface-name)
                  #'(method (... ...)))])))]))

(define-syntax (define-accessors stx)
  (syntax-parse stx
    [(_ prefix:id accessor ...)
      (let ([mk-name (lambda (name)
                       (format-id #'prefix "~a-~a" #'prefix name))])
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
      #`(define (axor-name self [or-else #f])
          (hash-get 'key self (or or-else (lambda () (error 'axor-name
                                                  "absent field: ~v" 'key)))))]
    [(_ axor-name:id key:id (param:id ...))
      #`(define (axor-name self param ...)
          ((hash-get 'key self (lambda ()
                                 (error 'axor-name "absent method: ~v" 'key)))
            param ...))]))

(define-syntax (define-form stx)
  (syntax-parse stx
    [(_ form (field:id ...) #:isa (iface:id ...) method ...)
      (make-define-form stx #'form #'(field ...) #'(iface ...) #'(method ...))]
    [(_ form (field:id ...) #:isa iface:id method ...)
      (make-define-form stx #'form #'(field ...) #'(iface) #'(method ...))]
    [(_ form (field:id ...) method ...)
      (make-define-form stx #'form #'(field ...) #'() #'(method ...))]))

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

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


(provide
  define-iface define-accessors define-accessor define-form
  record show-record)

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

(begin-for-syntax
  (define-syntax-class record-field
    (pattern (name:id value))
    (pattern ((name:id param:id ...) body ...)
      #:attr value #'(lambda (param ...) body ...))))

(define-syntax-parser (record binding:record-field ...)
  #`(let* ((binding.name binding.value) ...)
      (make-immutable-hash `((binding.name . ,binding.name) ...))))

;; Pretty-printing records.
(define (show-record r [indent 0])
  (define first #t)
  (define pre (make-string (* 2 indent) #\space))
  (define (next-line)
    (unless first (newline))
    (display pre)
    (display (if first "{ " "  "))
    (set! first #f))
  (for ([(key value) r]
        #:unless (procedure? value))
    (next-line)
    (printf "~a" (show key))
    (if (hash? value)
      (begin (newline) (show-record value (+ 1 indent)))
      (printf " ~a" (show value))))
  (define procs (for/list ([(name value) r]
                           #:when (procedure? value))
                  name))
  (if (null? procs)
    (display (if first (string-append pre "{}") " }"))
    (begin
      (next-line)
      (printf "methods: ~a }" procs)))
  (when (= 0 indent) (newline)))

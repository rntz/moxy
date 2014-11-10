#lang racket

(require
  (for-syntax
    (only-in racket/set list->set set-member?)
    syntax/parse racket/syntax
    "tags.rkt"))

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

(begin-for-syntax
  (define-syntax-class method
    (pattern (name:id value))
    (pattern ((name:id param:id ...) body ...)
      #:attr value #'(lambda (param ...) body ...))))

(define-syntax-parser (record binding:method ...)
  #`(let* ((binding.name binding.value) ...)
      (make-immutable-hash `((binding.name . ,binding.name) ...))))

(define-for-syntax (make-define-form stx form fields_ ifaces methods_)
  (define/syntax-parse (field:id ...) fields_)
  (define/syntax-parse (iface:id ...) ifaces)
  (define/syntax-parse (method:method ...) methods_)
  ;; FIXME: this is ugly
  (define (method-name a) (if (pair? a) (car a) a))
  (define all-methods (list->set (syntax->datum #'(field ... method.name ...))))
  (for* ([ifc (syntax->list #'(iface ...))]
         [m (map method-name (syntax-local-value ifc))]) ;ugliness here
    (unless (set-member? all-methods m)
      (raise-syntax-error #f (format "method ~v not implemented" m) stx)))
  #`(define (#,form field ...)
      (record [#,'form '#,form] [field field] ... method ...)))

(define-syntax-parser (define-iface iface accessor ...)
  #`(begin
      (define-syntax iface '(accessor ...))
      (define-accessors iface accessor ...)
      (define-syntax (#,(format-id #'iface "define-~a" #'iface) stx)
        (syntax-parse stx
          [(_ form:id (field:id (... ...)) method:method (... ...))
           (define iface-form (format-id #'form "~a:~a" #'iface #'form))
           (make-define-form stx iface-form
             #'(field (... ...)) #'(iface) #'(method (... ...)))]))))

(define-syntax-parser (define-accessors prefix:id accessor ...)
  #'(begin (define-accessor prefix accessor) ...))

(define-syntax-parser define-accessor
  [(_ prefix:id field:id)
    (define/with-syntax name (format-id #'prefix "~a-~a" #'prefix #'field))
    #`(define (name self [or-else #f])
        (hash-get 'field self
          (or or-else (lambda () (error 'name "absent field: ~v" 'field)))))]
  [(_ prefix:id (method:id param:id ...))
    (define/with-syntax name (format-id #'prefix "~a-~a" #'prefix #'method))
    #`(define (name self param ...)
        ((hash-get 'method self
           (lambda () (error 'name "absent method: ~v" 'method)))
          param ...))])

(define-syntax (define-form stx)
  (syntax-parse stx
    [(_ form (field:id ...) #:isa (iface:id ...) method ...)
      (make-define-form stx #'form #'(field ...) #'(iface ...) #'(method ...))]
    [(_ form (field:id ...) #:isa iface:id method ...)
      (make-define-form stx #'form #'(field ...) #'(iface) #'(method ...))]
    [(_ form (field:id ...) method ...)
      (make-define-form stx #'form #'(field ...) #'() #'(method ...))]))

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

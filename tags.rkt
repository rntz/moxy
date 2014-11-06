#lang racket

(require
  racket/provide-syntax
  (for-syntax racket/syntax syntax/parse))
(require "util.rkt")

;; Tag & annotated value interface
(provide
  (struct-out tag) make-tag!
  (struct-out tagged) make-tagged tagged-isa?
  define-tag define-tags tag-out
  (for-syntax tag-name-id tag-predicate-id tag-field-id
    tag-fields-id tag-fields))

;; omit-define-syntaxes necessary to allow (define-match-expander ann) later
(struct tag (name arity uid) #:transparent)
(struct tagged (tag value) #:transparent
  #:methods gen:custom-write
  [(define (write-proc self to-port mode)
     (define recur
       (match mode
         ['#t (lambda (x) (write x to-port))]
         ['#f (lambda (x) (display x to-port))]
         [(or '0 '1) (lambda (x) (print x to-port mode))]))
     (define t (tagged-tag self))
     (define name (tag-name t))
     (recur (if (= 0 (tag-arity t))
              name
              (cons name (vector->list (tagged-value self))))))])

(define (make-tag! name arity)
  (tag name arity (gensym name)))

(define (make-tagged t . args)
  (if (= (tag-arity t) (length args))
    (tagged t (apply vector-immutable args))
    (error "Tag arity does not match number of arguments")))

(define (tagged-isa? t obj)
  (and (tagged? obj) (equal? t (tagged-tag obj))))


;; Defines a new tag, along with a constructor & match-expander for it.
(begin-for-syntax
  (define (tag-name-id name) (format-id name "tag:~a" name))
  (define (tag-predicate-id name) (format-id name "~a?" name))
  (define (tag-field-id name field) (format-id name "~a-~a" name field))
  (define (tag-fields-id name) (format-id name "tag-fields:~a" name))
  (define (tag-fields name) (syntax-local-value (tag-fields-id name)))
  (define (acts-like x)
    (syntax-parser
      [(_:id args ...) #`(#,x args ...)]
      [_:id x])))

(define-syntax-parser (define-tag name:id field:id ...)
  (define tag-name   (tag-name-id #'name))
  (define name?      (tag-predicate-id #'name))
  (define fields     (tag-fields-id #'name))
  (define arity      (length (syntax->list #'(field ...))))
  (define ctor       (format-id #f "ctor:~a" #'name))
  #`(begin
      (define #,tag-name (make-tag! 'name #,arity))
      (define (#,name? x) (tagged-isa? #,tag-name x))
      (define-syntax #,fields '(field ...))
      (define #,ctor #,(if (= 0 arity)
                         #`(make-tagged #,tag-name)
                         #`(curry make-tagged #,tag-name)))
      (define-match-expander name
        ;; match expander
        (syntax-parser
          [(_ field ...)
            #'(tagged (? (curry equal? #,tag-name)) (vector field ...))])
        ;; regular expander
        (acts-like #'#,ctor))
      ;; for each field, define accessor
      #,@(for/list ([i (in-naturals 0)]
                    [f (syntax->list #'(field ...))])
           #`(define/contract (#,(tag-field-id #'name f) x)
               (-> #,name? any/c)
               (vector-ref (tagged-value x) #,i)))))

(define-syntax-rule (define-tags arg ...)
  (define-many define-tag arg ...))

(define-provide-syntax tag-out
  (syntax-parser
    [(_ name:id)
      #`(combine-out name
          #,(tag-name-id #'name)
          #,(tag-predicate-id #'name)
          #,(tag-fields-id #'name)
          #,@(for/list ([f (tag-fields #'name)])
               (format-id #'name "~a-~a" #'name f)))]
    [(_ name:id ...) #'(combine-out (tag-out name) ...)]))

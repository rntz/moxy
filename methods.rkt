#lang racket

(require "util.rkt")
(require "tags.rkt")

;; Implementation of single-dispatch no-inheritance methods. Dispatch is on
;; the tags of annotated values (see tags.rkt).


;; Representation of method objects. A human-readable name plus a unique
;; identifier.
(provide new-method method? method-name method-uid)

(struct method (name uid)
  #:transparent
  #:constructor-name make-method
  #:methods gen:equal+hash
  [(define (equal-proc a b recurse)
     (eq? (method-uid a) (method-uid b)))
   (define (hash-proc self recurse) (recurse (method-uid self)))
   (define (hash2-proc self recurse) (recurse (method-uid self)))])

(define (new-method name) (make-method name (gensym name)))


;; We use a table that maps (method . tag) pairs to map to an implementation of
;; that method for that tag.
;;
;; TODO: instead of a global variable in racket, pass this through the
;; interpreter somehow.
;;
;; TODO: make this weak (in that if either the tag or the method are GCed then
;; the entry will be removed).
(provide set-method! get-method apply-method call-method define-method!)

(define *method-table* (make-hash))

(define (get-method method tag)
  (hash-ref *method-table* (cons method tag)
    ;; TODO: better error message
    (lambda ()
      (error 'get-method "No implementation of method ~v for tag ~v"
        method tag))))

(define (set-method! method tag val)
  (hash-set! *method-table* (cons method tag) val))

(define (apply-method method val args)
  (apply (get-method method (ann-tag val)) val args))

(define (call-method method val . args) (apply-method method val args))

;; Convenience for defining methods
(define-syntax (define-method! stx)
  (syntax-case stx ()
    [(_ method tag value) #'(set-method! method tag value)]
    [(_ method tag (self params ...) body ...)
      #'(define-method! method tag (lambda (self params ...) body ...))]))

;; Naming convention for methods is a prefixed ^. For example:
;;
;;   (define ^name (new-method 'name))
;;   (define-method! ^name tag:Just (lambda (self) 'Just))

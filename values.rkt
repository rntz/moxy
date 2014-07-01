#lang racket

(require "util.rkt")

(provide
  make-ann ann ann-tag ann-args
  make-tag tag-name tag-uid tag-arity)

(struct ann (tag args) #:prefab)
(struct tag (name uid arity) #:prefab)

(define (make-tag name arity) (tag name (gensym name) arity))
(define (make-ann tag . args)
  (if (= (tag-arity tag) (length args))
    (ann tag (apply vector args))
    (error "Tag arity does not match number of arguments")))

#lang racket

(require (for-syntax racket/syntax))    ;format-id

(require "util.rkt")

(provide
  new-tag tag-name tag-uid tag-arity
  ann make-ann ann-tag ann-args
  )

;; omit-define-syntaxes necessary to allow (define-match-expander ann) later
(struct ann (tag args) #:prefab)

(struct tag (name uid arity field-map) #:prefab
  #:constructor-name make-tag)

(define (new-tag name field-names)
  (make-tag name (gensym name) (length field-names)
    (make-immutable-hash (zip-with cons field-names (in-naturals 0)))))

(define (tag-field-index tag field-name)
  (dict-ref (tag-field-map tag) field-name))

(define (make-ann tag . args)
  (if (= (tag-arity tag) (length args))
    (ann tag (apply vector-immutable args))
    (error "Tag arity does not match number of arguments")))

(define (ann-get-field ann field-name)
  (vector-ref
    (ann-args ann)
    (tag-field-index (ann-tag ann) field-name)))

;; TODO: equality for anns. or does 'equal? just work?

;; Make tagged values more easily match-able
;; note the single n, versus double n in "ann".
(define-syntax (define-tag stx)
  (with-syntax* ([(_ name fields ...) stx]
                 [tag-name (format-id stx "tag-~a" #'name)])
    (let ([field-list (syntax->datum #'(fields ...))])
      #`(begin
          (define tag-name (new-tag 'name '#,field-list))
          (define-match-expander name
            (lambda (stx1)
              (syntax-case stx1 ()
                [(_ fields ...)
                  #'(ann (? (partial equal? tag-name))
                         (vector fields ...))]))
            (lambda (stx1)
              (syntax-case stx1 ()
                #,@(if (= 0 (length field-list))
                     #'([(_ (... ...))
                          (error "invalid syntax: can't apply nullary tag")]
                        [_:id #'(make-ann tag-name)])
                     #'([(_ args (... ...))
                         #'(make-ann tag-name args (... ...))]
                        [_:id #'(lambda (fields ...)
                                  (make-ann tag-name fields ...))])))
              )))
      )))

;; Builtin tags.
(define-tag just value)
(define-tag none)

(define (maybe default inject v)
  (match v [(none) default] [(just x) (inject x)]))

(define (from-maybe default v) (maybe v identity v))

;; Hashtable interface. Modelled on
;; http://hackage.haskell.org/package/containers-0.5.5.1/docs/Data-Map-Strict.html
(provide hash-empty? hash-count)

(define (hash-has? k h) (hash-has-key? h k))
(define (hash-lookup k h)
  (if (hash-has? k h) (make-ann tag-just (hash-ref h k)) (make-ann tag-none)))

(define (hash-empty) (hash))
(define (hash-single k v) (hash k v))

(define (hash-get k h) (hash-ref k h (lambda () (error "key not in hash"))))
(define (hash-put k v h) (hash-set h k v))
(define (hash-put-with f k v h)
  (hash-put k (maybe v (partial f v) (hash-lookup k h)) h))

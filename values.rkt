#lang racket

(require (for-syntax racket/syntax))    ;format-id
(require racket/generic)                ;for gen:custom-write
(require (only-in racket [hash-map racket/hash-map]))

(require "util.rkt")


;; Hashtable interface. Modelled on
;; http://hackage.haskell.org/package/containers-0.5.5.1/docs/Data-Map-Strict.html
;;
;; Needs to be defined here for access to Just, None.
;; TODO: more functionality
(provide
  hash-empty? hash-count                ;re-exports
  hash-empty hash-single hash-from-list hash-from-keys-values
  hash-has? hash-lookup hash-get
  hash-put hash-put-with hash-delete hash-alter
  hash-map hash-union)

(define (hash-empty) (hash))
(define (hash-single k v) (hash k v))
(define (hash-from-list kvs)
  ;; convert a list-list like ((a x) (b y)) to a cons-list ((a . x) (b . y)).
  (make-immutable-hash (map (lambda (x) (apply cons x)) kvs)))
(define (hash-from-keys-values keys values)
  (hash-from-list (zip keys values)))

(define (hash-has? k h) (hash-has-key? h k))
(define (hash-lookup k h)
  (if (hash-has? k h) (Just (hash-ref h k)) None))
(define (hash-get k h [or-else #f])
  (cond
    [(procedure? or-else) (hash-ref h k or-else)]
    [(not or-else) (hash-ref h k)]
    [else
      (error "or-else argument to hash-get must be a procedure or #f")]))

(define (hash-put k v h) (hash-set h k v))
(define (hash-put-with k v h f)
  (hash-put k (maybe (hash-lookup k h) v (lambda (x) (f k x v)))
            h))

(define (hash-delete k h) (hash-remove h k))

;; f takes (Maybe v) -> (Maybe v)
(define (hash-alter k h f)
  (match (f (hash-lookup k h))
    [(None) (hash-delete k h)]
    [(Just x) (hash-put k x h)]))

;; (f k v) --> new-v
(define (hash-map h f)
  (make-immutable-hash
    (racket/hash-map h (lambda (k v) (cons k (f k v))))))

(define (hash-union a b [combine (lambda (k x y) y)])
  (cond
    [(hash-empty? a) b]
    [(hash-empty? b) a]
    [else
      (hash-for-each b
        (lambda (key val) (set! a (hash-put-with key val a combine))))
      a]))


;; Tag & annotated value interface
(provide
  new-tag tag? tag-name tag-uid tag-arity tag-field-index
  ann ann? make-ann ann-tag ann-args ann-get-field ann-isa?
  define-tag)

;; omit-define-syntaxes necessary to allow (define-match-expander ann) later
(struct ann (tag args)
  #:transparent
  #:methods gen:custom-write
  ;; mode is #t for 'write, #f for 'display, or 0 or 1 (indicating quoting
  ;; depth) for 'print.
  [(define (write-proc self to-port mode)
     (let* ([recur (match mode
                    ['#t (lambda (x) (write x to-port))]
                    ['#f (lambda (x) (display x to-port))]
                    [(or '0 '1) (lambda (x) (print x to-port mode))])]
            [tag (ann-tag self)]
            [name (tag-name tag)])
       ;; this completely ignores mode, because I just don't care.
       (recur
         (if (= 0 (tag-arity tag))
           name
           (cons name (vector->list (ann-args self)))))))])

(struct tag (name uid arity field-map) #:prefab
  #:constructor-name make-tag)

(define (new-tag name field-names)
  (make-tag name (gensym name) (length field-names)
    (hash-from-keys-values field-names (in-naturals 0))))

(define (tag-field-index tag field-name)
  ;; TODO: better error message on failure
  (hash-get field-name (tag-field-map tag)
    (lambda () "tag has no such field")))

(define (make-ann tag . args)
  (if (= (tag-arity tag) (length args))
    (ann tag (apply vector-immutable args))
    (error "Tag arity does not match number of arguments")))

(define (ann-get-field ann field-name)
  (vector-ref
    (ann-args ann)
    (tag-field-index (ann-tag ann) field-name)))

(define (ann-isa? tag ann)
  (and (ann? ann) (equal? tag (ann-tag ann))))


;; Defines a new tag, along with a constructor & match-expander for it.
(define-syntax (define-tag stx)
  (with-syntax* ([(_ name fields ...) stx]
                 [tag-name (format-id stx "tag:~a" #'name)]
                 [name?    (format-id stx "~a?" #'name)])
    (let ([field-list (syntax->datum #'(fields ...))])
      #`(begin
          (define tag-name (new-tag 'name '#,field-list))
          (define (name? x) (ann-isa? tag-name x))
          ;; Define pattern-matcher & constructor
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
                                  (make-ann tag-name fields ...))])))))
          ;; For each field, define accessor
          #,@(for/list ([i (in-naturals 0)]
                        [field (syntax->list #'(fields ...))])
               (with-syntax ([f field]
                             [accessor (format-id stx "~a-~a" #'name field)])
                 #`(define (accessor x)
                     (if (name? x)
                       (vector-ref (ann-args x) '#,i)
                       ;; TODO: better error message
                       (error "invalid use of accessor")))))))))


;; Builtin tags.
(provide
  tag:True True True? tag:False False False? truthy? falsey? truthify
  tag:Just Just Just? Just-value
  tag:None None None?
  maybe? maybe from-maybe maybe-bind maybe-map maybe-filter
  tag:Monoid Monoid Monoid? Monoid-join Monoid-empty
  tag:ExtPoint ExtPoint ExtPoint? ExtPoint-name ExtPoint-uid ExtPoint-monoid
  ExtPoint-join ExtPoint-empty)

;; booleans
(define-tag True)
(define-tag False)
(define (falsey? x)
  (if (not (boolean? x)) (False? x)
    (error "never call falsey? on a boolean!")))
(define (truthy? x) (not (falsey? x)))
(define (truthify x)
  (if (boolean? x) (if x True False)
    (error "never call truthify on a non-boolean!")))

;; maybe
(define-tag Just value)
(define-tag None)

(define (maybe? x) (or (Just? x) (None? x)))

(define (maybe v default inject)
  (match v [(None) default] [(Just x) (inject x)]))

(define (from-maybe v default) (maybe v default identity))

(define (maybe-bind v f) (maybe v None f))
(define (maybe-map v f) (maybe-bind v (lambda (x) (Just (f x)))))
(define (maybe-filter v ok?)
  (match v
    [(Just x) (if (ok? x) v None)]
    [(None) v]))

(define-tag Monoid join empty)
(define-tag ExtPoint name uid monoid)

(define ExtPoint-join (compose Monoid-join ExtPoint-monoid))
(define ExtPoint-empty (compose Monoid-empty ExtPoint-monoid))

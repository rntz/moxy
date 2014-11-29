#lang racket

;;(require (only-in racket [hash-map racket/hash-map]))

(require "util.rkt")
(require "tags.rkt")


;; Hashtable interface. Modelled on
;; http://hackage.haskell.org/package/containers-0.5.5.1/docs/Data-Map-Strict.html
;;
;; Needs to be defined here for access to Just, None.
;; TODO: more functionality
(provide
  hash-empty? hash-count                ;re-exports
  hash-empty hash-single hash-from-list hash-from-keys-values
  hash-has? hash-lookup hash-get
  hash-put hash-delete
  ;; hash-put-with hash-alter hash-map
  hash-union)

(define hash-empty (hash))
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
    [else (error "or-else argument to hash-get must be a procedure or #f")]))

(define (hash-put k v h) (hash-set h k v))
;; (define (hash-put-with k v h f)
;;   (hash-put k (maybe (hash-lookup k h) v (lambda (x) (f k x v))) h))

(define (hash-delete k h) (hash-remove h k))

;; ;; f takes (Maybe v) -> (Maybe v)
;; (define (hash-alter k h f)
;;   (match (f (hash-lookup k h))
;;     [(None) (hash-delete k h)]
;;     [(Just x) (hash-put k x h)]))

;; ;; (f k v) --> new-v
;; (define (hash-map h f)
;;   (make-immutable-hash
;;     (racket/hash-map h (lambda (k v) (cons k (f k v))))))

(define (hash-union a b [combine (lambda (k x y) y)])
  (for/fold ([a a])
            ([(key v2) b])
    (hash-update a key (lambda (v1) (combine key v1 v2)) (lambda () v2))))


;; Builtin tags.
(provide
  (tag-out L R True False Just None Ok Err Monoid ExtPoint)
  truthy? falsey? truthify
  maybe? maybe from-maybe maybe-bind maybe-map maybe-filter Maybe/c
  ExtPoint-join ExtPoint-empty)

;; directions. built-in for associativity purposes.
(define-tags L R)

;; booleans
(define-tags True False)
(define (falsey? x)
  (if (not (boolean? x)) (False? x)
    (error "never call falsey? on a boolean!")))
(define (truthy? x) (not (falsey? x)))
(define (truthify x)
  (if (boolean? x) (if x True False)
    (error "never call truthify on a non-boolean!")))

;; maybe
(define-tags None (Just value))

(define (maybe? x) (or (Just? x) (None? x)))
(define (Maybe/c c)
  (or/c None?
    (struct/c tagged
      (lambda (x) (equal? tag:Just x))
      (vector-immutable/c c))))

(define (maybe v default inject)
  (match v [(None) default] [(Just x) (inject x)]))

(define (from-maybe v default) (maybe v default identity))

(define (maybe-bind v f) (maybe v None f))
(define (maybe-map v f) (maybe-bind v (lambda (x) (Just (f x)))))
(define (maybe-filter v ok?)
  (match v
    [(Just x) (if (ok? x) v None)]
    [(None) v]))

(define-tags (Ok value) (Err value))

;; TODO: eliminate Monoid, just have ExtPoints?
(define-tag Monoid join empty)
(define-tag ExtPoint name uid monoid)

(define ExtPoint-join (compose Monoid-join ExtPoint-monoid))
(define ExtPoint-empty (compose Monoid-empty ExtPoint-monoid))

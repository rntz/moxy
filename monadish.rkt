#lang racket

(require (prefix-in list- (only-in racket last)))

(require "util.rkt")
(require (only-in "values.rkt" hash-union))
(require "objects.rkt")

;; We represent structures as hashes from symbols, naming methods, to
;; functions that implement them.

;; An idiom/applicative F has the following interface:
;; - (pure x) for x:A --> F A
;; - (map f x1 ... xn)
;; - (ap f x1 ... xn)
;; - (lift f) for f:(A1...An) -> B --> (F A1 ... F An) -> F b
;; - (seq a) for a:(listof (F A)) --> F (listof A)
;; - (last a1 ... an)

(provide Idiom-pure Idiom-map Idiom-ap Idiom-lift Idiom-seq Idiom-last idiom)

(define-iface Idiom pure map ap lift seq last)

(define (idiom #:lift [lift_ #f] #:map [map_ #f]
               #:pure [pure_ #f] #:ap [ap_ #f]
               #:seq [seq_ #f] #:last [last_ #f])
  (record
    [lift (or lift_
            (and map_ (lambda (f) (lambda as (apply map_ f as))))
            (and pure_ ap_ (lambda (f) (lambda as (apply ap_ (pure_ f) as))))
            (error "cannot make idiom without lift, map, or pure + ap"))]
    [map  (or map_  (lambda (f . as) (apply (lift f) as)))]
    [pure (or pure_ (lambda (x) (map (const x))))]
    [ap   (or ap_   (lift funcall))]
    ;; WARNING: will be horrible on long lists
    [seq  (or seq_ (lambda (l) (apply (lift list) l)))]
    [last (or last_ (lambda as (map list-last (seq as))))]))

;; A monad F has the following as well:
;; - (join k) for k:F (F A) --> F A
;; - ((bind f) k1 ... kn)
;;   note: order of arguments differs from usual convention!

(provide Monad-join Monad-bind lift/join->monad pure/bind->monad)

(define-iface Monad join bind)

(define ((bind->join bind) k) ((bind identity) k))
(define (((lift/join->bind lift join) f) . ks) (join (apply (lift f) ks)))

(define (lift/join->monad lift join)
  (hash-union
    (record [join join] [bind (lift/join->bind lift join)])
    (idiom #:lift lift)))

(define (pure/bind->monad pure bind)
  (hash-union
    (record [bind bind] [join (bind->join bind)])
    (idiom
      #:pure pure
      #:lift (lambda (f) (bind (compose pure f))))))

;; An alternative F has the following:
;; - zero : F A
;; - fail : string -> F A
;; - (plus x y) for x:F A, y:F A -> F A

(provide Alternative-zero Alternative-fail Alternative-plus)

(define-iface Alternative zero fail plus)


;; Some monadishes.
(provide id-monad reader-monad reader/ask reader/local univ-monad)

(define id-monad
  (hash-union
    (idiom #:lift identity #:pure identity #:seq identity
      #:map funcall #:ap funcall)
    (hash 'bind funcall 'join identity)))

;; Reader
(define (((reader/lift f) . as) . reader-args)
  (apply f (map (lambda (k) (apply k reader-args)) as)))

(define ((reader/join k) . args) (apply (apply k args) args))

(define reader-monad (lift/join->monad reader/lift reader/join))

;; TODO: perhaps these outta work with multiple arguments/returns?
(define reader/ask identity)
(define ((reader/local f k) x) (k (f x)))

;; The "universal" (initial) monad: produces a function that takes a monad as
;; argument and runs the computation in that monad. In some sense, a flavor of
;; reader monad.
(define ((univ/join k) mish) ((k mish) mish))
(define (((univ/lift f) . as) mish)
  (apply ((Idiom-lift mish) f) (map (call-with mish) as)))

(define univ-monad (lift/join->monad univ/lift univ/join))

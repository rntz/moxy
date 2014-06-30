;; #lang racket

(struct extension-point (uid name empty join)
  #:constructor-name make-extension-point-internal
  #:methods
  gen:equal+hash
  [(define (equal-proc a b equal?-recur)
     (eq? (extension-point-uid a) (extension-point-uid b)))
   (define (hash-proc a hash-recur)
     (hash-recur (extension-point-uid a)))
    (define (hash2-proc a hash-recur)
      (hash-recur (extension-point-uid a)))])

(define (make-extension-point name empty join)
  (make-extension-point-internal (gensym name) name empty join))

(define-syntax-rule (define-extension-point name empty join)
  (define name (make-extension-point (quote name) empty join)))

;; Environments are immutable hashtables, mapping extension-points to their
;; monoid values. If an extension-point is absent, it is the same as being
;; mapped to its empty value.
(define env-empty (hash))

(define (env-join-2 a b)
  (cond
    [(dict-empty? a) b]
    [(dict-empty? b) a]
    [else
      ;; god this is inefficient
      (dict-for-each b
        (lambda (k v2)
          (set! a (dict-update a k
                    (lambda (v1) ((extension-point-join k) v1 v2))
                    (extension-point-empty k)))))
      a]))

(define/match (env-join . es)
  [('()) env-empty]
  [(`(,a)) a]
  [(`(,a ,b)) (env-join-2 a b)]
  [((cons a as)) (foldl env-join-2 a as)])

(define (env-make ext-point value)
  (hash ext-point value))

(define (env-get ext ext-point)
  (dict-ref ext ext-point (extension-point-empty ext-point)))

(displayln "env.rkt loaded")

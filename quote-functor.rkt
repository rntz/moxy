#lang racket

(define (writeln x) (write x) (newline))

(define (((curry f) . as) . bs) (apply f (append as bs)))
(define ((partial f . as) . bs) (apply f (append as bs)))
(define ((nary f) . as) (f as))
(define ((unary f) xs) (apply f xs))

;; true for things that evaluate to themselves
(define (literal? x) (or (number? x) (string? x) (procedure? x)))

;; (define (magic-list n) (if (= 0 n) list 'list))

;; for monadic x, we have the following invariant:
;;
;;     (eval^n (x n)) = (eval^m (x m))
;;
;; for any natural n,m.
;; for m = 0: (eval^n (x n)) = (x 0)
;;
;; TODO: monadic join, bind. currently only implements an applicative functor.

(define ((pure x) n)
  (when (< n 0) (error 'pure "invalid quote level: ~a" n))
  (if (or (= 0 n) (literal? x)) x
    ((pure `',x) (- n 1))))

(define (ap f . xs) (ap* f xs))
(define ((ap* f xs) n)
  (ap-inner n (f n) (map (lambda (x) (x n)) xs)))
(define (ap-inner n f xs)
  (if (= 0 n) (apply f xs)
    (ap-inner (- n 1) list (cons f xs))))

;; (define (ap-inner n f xs)
;;   (if (= 0 n) (apply f xs)
;;     ((iter (- n 1) (partial cons 'list)) (cons f xs))))

(define (fmap f . xs) (fmap* f xs))
(define (fmap* f xs) (ap* (pure f) xs))
(define (lift f) (partial fmap f))

(define (seq* as) (fmap* list as))
;; (define (seq* as) (ap* magic-list as))
(define seq (nary seq*))

(define <$> fmap)
(define <*> ap)

(define (ask n) ((pure n) n))      ; thanks to literal optimization, (ask n) = n

(define ((quo x) n) (x (+ 1 n)))
(define ((unq x) n)
  (if (= 0 n) (error "invalid downshift")
    (x (- n 1))))

(define (iter n f)
  (apply compose (build-list n (const f))))

(define (run x [n 0])
  ((iter n eval) (x n)))


;; example
(define (tr x [env '()])
  (match x
    ;; special forms
    [`(lambda ,(and vs (list (? symbol?) ...)) ,body)
      (let ((names (map gensym vs)))
        (define (f x) `(lambda (,@names) ,x))
        (<$> f (tr body (append (map cons vs names) env))))]
    [`(lambda . ,_) (error 'tr "bad lambda")]
    [`(quote ,e) (pure (list 'quote e))]
    [`(,'quasiquote ,e)
      (quo (tr e env))]
    [`(,'unquote ,e)
      (unq (tr e env))]
    ;; atoms
    ['() (error 'tr "empty list")]
    [(? number?) (pure x)]
    [(? symbol?)
      (pure
        (let ((entry (assoc x env)))
          (if entry (cdr entry) x)))]
    ;; function application
    [(? pair?)
      (seq* (map (lambda (x) (tr x env)) x))]))

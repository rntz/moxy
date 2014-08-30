#lang racket

;; This is some old work on implementing quasiquoting as a monad-ish thing
;; (currently, only an applicative functor). It uses functions to represent
;; quasiquoted thunks rather than actual racket code; this makes unquotation
;; difficult. I wasn't able to get it working. Proceed at your own risk.

(define (writeln x) (write x) (newline))

(define (((curry f) . as) . bs) (apply f (append as bs)))
(define ((partial f . as) . bs) (apply f (append as bs)))
(define ((nary f) . as) (f as))
(define ((unary f) xs) (apply f xs))
(define ((flip f) x y) (f y x))

(define ((pure x) n)
  (if (= 0 n) x
    ((pure (const x)) (- n 1))))

(define ((ap f x) n)
  (ap-inner n (f n) (x n)))

(define (ap-inner n f x)
  (if (= 0 n) (f x)
    (lambda a (ap-inner (- n 1) (apply f a) (apply x a)))))

(define (map1 f x) (ap (pure f) x))
(define (map2 f x y) (ap (map1 (curry f) x) y))
(define (lift1 f) (partial map1 f))
(define (lift2 f) (partial map2 f))

(define (seq as) (foldr (lift2 cons) (pure '()) as))
(define seq* (nary seq))

(define (<$> f . as) (map1 (unary f) (seq as)))
(define (<*> f . as) (map1 (lambda (f . as) (apply f as)) (seq (cons f as))))

(define (ask n) ((pure n) n))

;; weirdness
(define ((up x) n) (x (+ 1 n)))
(define ((down x) n)
  (if (= 0 n) (error "invalid downshift")
    (x (- n 1))))

(define (unq f)
  (down (lambda (n) (lambda (a) ((f a) n)))))

;; I don't know which nesting behavior is correct, but hopefully it's one of
;; these two!
(define quo1 up)

(define ((quo2 x) n) (quo-inner n (x (+ 1 n))))
(define (quo-inner n x)
  (if (= 0 n) x
    (lambda (a) (quo-inner (- n 1) (lambda (z) ((x z) a))))))

(define quo quo2)                       ;for now, let's use quo2

(define (subst v x) (ap (quo x) (pure v)))

(define (run x . as)
  (foldl
    (lambda (arg func) (func arg))
    (x (length as))
    as))


;;; this is useless
(define (parse x [env '()])
  (cond
    [(symbol? x) (pure `(var ,x))]
    [(or (number? x) (string? x))
      (pure `(lit ,x))]
    [(pair? x)
      (match x
        [`(lambda ,params ,body)
          (seq* (pure 'lambda) (pure params) (parse body))]
        [`(,'unquote ,e)
          (unq (lambda (x) (seq* (pure x) (parse e))))]
        [`(,'quote ,x)
          (quo (parse x))]
        [_ (map1 (partial cons 'call) (seq (map parse x)))])]))

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
      (ap (quo (tr e env)) (pure env))]
    ;; atoms
    ['() (error 'tr "empty list")]
    [(? number?) (pure x)]
    [(? symbol?)
      (pure
        (let ((entry (assoc x env)))
          (if entry (cdr entry) x)))]
    ;; function application
    [(? pair?)
      (seq (map (lambda (x) (tr x env)) x))]
    [(? pair?)
      (let loop ([as (reverse x)])
        (match as
          [`(,a) (tr a env)]
          [`(,a . ,as)
            (seq* (loop as) (tr a env))]))]))


;;; old stuff
;; ;; quo : Q b -> Q (a -> b)
;; (define ((quo x) n)
;;   ;; this is wrong
;;   ((pure (lambda (a) ((x (+ 1 n)) a))) n))

;; (define ((subst v x) n)
;;   ((x (+ 1 n)) v))

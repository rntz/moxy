;; Compiling language into Racket.
;; quick-and-dirty. likely to have bugs.

(define (compile-expr e)
  (match e
    [`(lit ,lit) lit]
    [`(id ,sym) sym]
    [`(list ,exprs) `(,list ,@(map compile-expr exprs))]
    [`(list ,exprs ,end)
      (foldr (lambda (x y) `(,cons ,x ,y))
        (compile-expr end)
        (map compile-expr exprs))]
    [`(lambda (,params . ,rest-param) ,body)
      (compile-lambda params rest-param body)]
    [`(call ,fnc ,args)
      `(,(compile-expr fnc) ,@(map compile-expr args))]
    [`(call ,fnc ,args ,rest-arg)
      `(,apply
         ,(compile-expr fnc)
         ,@(map compile-expr args)
         ,(compile-expr rest-arg))]
    [`(block ,block) (compile-block block)]
    [`(if ,cnd ,thn ,els)
      `(if ,(compile-expr cnd) ,(compile-block thn) ,(compile-block els))]
    ;; unimplemented cases
    [`(dict . ,_) (raise "dict unimplemented")]
    [`(return . ,_) (raise "return unimplemented")]))

(define (compile-lambda params rest-param body)
  `(lambda (,@params ,@rest-param) ,(compile-block body)))

(define (compile-block b)
  ;; We lift all variable scopes to front, but they aren't actually initialized
  ;; until the corresponding decl is reached.
  (define/match (decl-vars d)
      [('empty) '()]
      [(`(expr ,_)) '()]
      [(`(let ,id ,_)) (list id)]
      [(`(fn ,id . ,_)) (list id)])
  (define/match (compile-decl d)
    [(`empty) (void)]
    [(`(expr ,e)) (compile-expr e)]
    [(`(let ,id ,exp)) `(set! ,id ,(compile-expr exp))]
    [(`(fn ,id (,params . ,rest-param) ,body))
      `(set! ,id ,(compile-lambda params rest-param body))])
  `(let (,@(map (lambda (x) `[,x ,(void)]) (append-map decl-vars b)))
     ,@(map compile-decl b)))

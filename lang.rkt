(require racket/stream)

;; TODO: use the racket module system for these things
(load "lex.rkt")
(load "parse.rkt")

;; Utility wrapper
(define (parse-with parser port)
  ((<* parser peof)
    (void)
    (stream-stream (stream-map position-token-token (tokenize port)))
    (lambda (loc msg) `(hard ,loc ,msg))
    (lambda (loc msg) `(soft ,loc ,msg))
    (lambda (_ r) `(ok ,r))))

(define (tag n . as) (apply <$> (partial list n) as))
(define (keyword id) (expect `(ident ,id)))
(define (keysym id) (expect `(symbol ,id)))

;;; Atoms.
(define comma (expect `(symbol ",")))
(define dot (expect `(symbol ".")))
(define semi (expect `(symbol ";")))

(define lparen (expect 'lparen)) (define rparen (expect 'rparen))
(define lbrace (expect 'lbrace)) (define rbrace (expect 'rbrace))
(define lbracket (expect 'lbracket)) (define rbracket (expect 'rbracket))

(define (parens x) (between lparen rparen x))
(define (braces x) (between lbrace rbrace x))
(define (brackets x) (between lbracket rbracket x))

(define (token-of type)
  (fmap1 cadr (satisfy (lambda (x) (and (pair? x)
                                        (eq? (car x) type))))))

(define p-string (token-of 'string))
(define p-number (token-of 'number))
(define p-ident (<$> string->symbol (token-of 'ident)))

(define p-lit (tag 'lit (choice p-string p-number)))
(define p-atom (choice (tag 'id p-ident) p-lit))

;; Parses an expression.
;; TODO: infix operators
(define p-expr (eta p-funcall))

;; A comma-separated-list of ps, except that ";" can be used to indicate a cons
;; at the very end. For example, "1,2;3". Also, trailing commas are allowed, as
;; in "1,2,3,".
(define (p-listish p)
  (<$> cons
    (sep-end-by p comma)
    (option '() (fmap1 list (*> semi p)))))

(define p-list
  (between lbracket rbracket
    (<$> (lambda (x) `(list ,@x))
         (p-listish p-expr))))

(define p-pat p-ident)

(define p-let-decl
  (tag 'let
    (*> (keyword "let") p-pat)
    (*> (keysym "=") p-expr)))

(define p-params (p-listish p-ident))

(define p-fun-decl
  (tag 'fn
    (*> (keyword "fn") p-ident)
    (parens p-params)
    (eta p-block)))

(define p-decls
  (choice
    (<$> cons p-let-decl (*> semi (eta p-decls)))
    (<$> cons p-fun-decl (eta p-decls))
    (<$> cons (tag 'expr p-expr)
              (option '() (*> semi (eta p-decls))))
    (return '(empty))))

(define p-block (between lbrace rbrace p-decls))

(define p-lambda
  (tag 'lambda
    (*> (keysym "\\") p-params)
    p-block))

(define p-else
  (choice p-block
    (<$> (lambda (exp) `((expr ,exp))) (eta p-if))))

(define p-if
  (tag 'if
    (*> (keyword "if") p-expr)
    p-block
    (option '(empty) (*> (keyword "else") p-else))))

;; Parses an expr that isn't a function call.
;; TODO: return, case, dict literals
(define p-nocall
  (choice (parens p-expr) (tag 'block p-block)
    p-if p-lambda p-list p-atom))

(define p-args (parens (p-listish p-expr)))

(define (mk-funcalls func argses)
      (foldl (lambda (args f) `(call ,f ,@args)) func argses))

;; Parses an expr of the form
;; > func(args1...)(args2...)...(argsN...)
;; for any number N of argument lists (including 0).
(define p-funcall
  (<$> mk-funcalls p-nocall (many p-args)))

;; Format of s-expr representations:
;; EXPR
;; ::= (lit LIT)
;;   | (id SYMBOL)
;;   | (list (EXPR*) EXPR?)
;;   | (dict ((EXPR EXPR)*))
;;   | (lambda ((VAR*) VAR?) BLOCK)
;;   | (call EXPR (EXPR*) EXPR?)
;;   | (block BLOCK)
;;   | (if EXPR BLOCK BLOCK)
;;   | (return EXPR)
;;   -- TODO: case-expression
;;
;; BLOCK ::= (DECL* empty?)
;;
;; DECL
;; ::= (let PAT EXPR)
;;   | (fn NAME ((VAR*) VAR?) BLOCK)
;;   | (expr EXPR)
;;
;; PAT ::= SYMBOL

(displayln "loaded lang.rkt")

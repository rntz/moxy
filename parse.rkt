#lang racket

(require racket/stream)
(require (only-in parser-tools/lex position-token-token))

(require "util.rkt")
(require "lex.rkt")
(require "pcomb.rkt")
(require "values.rkt")
(require "env.rkt")

;;; TODO: provide

;; Utility wrapper
(define (parse-with parser port)
  ((<* parser peof)
    (void)
    (stream-stream (stream-map position-token-token (tokenize port)))
    (lambda (loc msg) `(hard ,loc ,msg))
    (lambda (loc msg) `(soft ,loc ,msg))
    (lambda (_ r) `(ok ,r))))

;; (define (tag n . as) (apply <$> (partial list n) as))
(define (keyword id) (expect (TID id)))
(define (keysym id) (expect (TSYM id)))

;;; Atoms.
(define comma (keysym ","))
(define dot (keysym "."))
(define semi (keysym ";"))

(define lparen (expect TLPAREN)) (define rparen (expect TRPAREN))
(define lbrace (expect TLBRACE)) (define rbrace (expect TRBRACE))
(define lbrack (expect TLBRACK)) (define rbrack (expect TRBRACK))

(define (parens x) (between lparen rparen x))
(define (braces x) (between lbrace rbrace x))
(define (brackets x) (between lbrack rbrack x))

(define p-str (<$> TSTR-value (satisfy TSTR?)))
(define p-num (<$> TNUM-value (satisfy TNUM?)))
(define p-id (<$> (compose string->symbol TID-value) (satisfy TID?)))

;; (define p-lit (tag 'lit (choice p-str p-num)))
;; (define p-atom (choice (tag 'id p-id) p-lit))

;; A comma-separated-list, except leading and/or trailing commas are allowed.
(define (p-listish p) (begin-sep-end-by p comma))

;; Precedence levels, taken largely from Haskell (larger binds tighter than
;; smaller, right binds tighter than left):
;;
;;   infixr 0 $
;;   infixr 2 ||
;;   infixr 3 &&
;;   infixr 5 : ++
;;   infixl 6 + -
;;   infixl 7 * /
;;   infixr 8 ^
;;   infixl 10 function application f(x,y,z)

;; Problem: precedence not taken into account. "let" does not have same
;; precedence as function application. Is this a real problem?
;;
;; i.e. some extensions shouldn't be applicable in head position:
;;
;;    let x = 2 in x(1,2,3)
;;
;; should never parse as "(let x = 2 in x)(1,2,3)". I think this will never
;; happen in practice due to greedy-ness, but it's worrying.
(define p-from-@exprs
  (>>=
    ask                                ; grab the extensible parsing environment
    (lambda (parse-env)
      ;; Grab a token and look it up in @exprs. Fail soft if it's absent.
      (try-one-maybe
        (lambda (t) (hash-lookup t (env-get @exprs parse-env)))))
    ;; Run the parser we found in @exprs!
    (lambda (parser) parser)))

;; Parses an expression.
(define p-expr (eta (p-expr-at 10)))

;; TODO: if, lambda, list, return, case, dict literals

(define (p-expr-at prec)
  (>>= (p-prefix-expr-at prec)
       (lambda (e) (p-infix-expr prec e))))

(define (p-prefix-expr-at prec)
  (choice
    (<$> expr:lit (choice p-str p-num))
    p-from-@exprs
    ;; an identifier. TODO: shouldn't we exclude identifiers bound to parse
    ;; extensions from this?
    (<$> expr:var p-id)))

(define (p-infix-expr prec left-expr)
  ;; ask @infix-exprs what to do
  ;; TODO: should we have a separate @infix-ops?
  (>>= ask
       (lambda (parse-env)
         (try (try-one-maybe
                (lambda (t) (hash-lookup t (env-get @infix-exprs parse-env))))))
       (lambda (infix-expr)
         (error "unimplemented")        ;FIXME
         )))

;; (define (mk-funcalls func argses)
;;   (foldl (lambda (args f) (expr:call f args)) func argses))

;; (define p-expr-args (parens (p-listish p-expr)))

;; Parses an expr of the form
;; > func(args1...)(args2...)...(argsN...)
;; for any number N of argument lists (including 0).
;; (define p-funcall
;;   (<$> mk-funcalls p-expr-call-head (many p-expr-args)))

;; (define p-let-decl
;;   (tag 'let
;;     (*> (keyword "let") p-pat)
;;     (*> (keysym "=") p-expr)))

;; (define p-params (p-listish p-ident))

;; (define p-fun-decl
;;   (tag 'fn
;;     (*> (keyword "fun") p-ident)
;;     (parens p-params)
;;     (eta p-block)))

;; (define p-decls
;;   (choice
;;     (<$> cons p-let-decl (*> semi (eta p-decls)))
;;     (<$> cons p-fun-decl (eta p-decls))
;;     (<$> cons (tag 'expr p-expr)
;;               (option '() (*> semi (eta p-decls))))
;;     (return '(empty))))

;; (define p-block (between lbrace rbrace p-decls))

;; (define p-lambda
;;   (tag 'lambda
;;     (*> (keysym "\\") p-params)
;;     p-block))

;; (define p-else
;;   (choice p-block
;;     (<$> (lambda (exp) `((expr ,exp))) (eta p-if))))

;; (define p-if
;;   (tag 'if
;;     (*> (keyword "if") p-expr)
;;     p-block
;;     (option '(empty) (*> (keyword "else") p-else))))

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

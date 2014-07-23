#lang racket

(require racket/stream)
(require (only-in parser-tools/lex position-token-token))

(require "util.rkt")
(require "lex.rkt")
(require "pcomb.rkt")
(require "values.rkt")
(require "env.rkt")

(provide parse parse-all
  builtin-parse-env
  p-expr p-decl p-decls ;; TODO: p-toplevel-decl
  )

(define (parse parser env what)
  (parser
    env
    (stream-stream
      (cond
        [(port? what) (tokenize what)]
        [(string? what) (call-with-input-string what tokenize)]
        [#t (error 'parse "don't know how to parse: ~v" what)]))
    (lambda (loc msg) (error 'parse "hard error at pos ~a: ~a" loc msg))
    (lambda (loc msg) (error 'parse "soft error at pos ~a: ~a" loc msg))
    (lambda (_ r) r)))

(define (parse-all parser env what)
  (parse (<* parser peof) env what))


;; Utility thingies
(define (keyword id) (expect (TID id)))
(define (keysym id) (expect (TSYM id)))

(define comma (keysym ","))
(define dot (keysym "."))
(define semi (keysym ";"))

(define p-end (keyword "end"))
(define p-optional-end (optional p-end))

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
    (lambda (ext) (@expr-parser ext))))

;; Parses an expression.
(define (p-expr-at prec)
  (>>= (p-prefix-expr-at prec)
    (lambda (e) (p-infix-expr prec e))))

;; This doesn't use `prec', but maybe in future it will?
(define (p-prefix-expr-at prec)
  (choice
    (<$> expr:lit (choice p-str p-num))
    p-from-@exprs
    ;; an identifier. TODO: shouldn't we exclude identifiers bound to parse
    ;; extensions from this?
    (<$> expr:var p-id)))

;; Note on infix precedence: Larger precedences bind tighter than smaller, and
;; right-associative binds tighter than left-associative.

;; Tries to parse an infix continuation for `left-expr' of precedence at least
;; `prec' (i.e. binding at least as tightly as `prec').
(define (p-infix-expr prec left-expr)
  ;; TODO: should we have a separate @infix-ops?
  (option left-expr
    (>>= ask
      (lambda (parse-env)
        (try-one-maybe
          ;; Look for an infix operator associated with the token `t' whose
          ;; precedence is at least `prec' (i.e. as tight or tighter-binding as
          ;; what we're currently looking for).
          (lambda (t) (filter-maybe
                   (hash-lookup t (env-get @infixes parse-env))
                   (lambda (x) (<= prec (@infix-precedence x)))))))
      (lambda (ext)
        ;; Pass off to their parser
        (@infix-parser ext left-expr))
      ;; Try to keep parsing more operations afterward.
      ;;
      ;; Note: This handles left-associativity automatically. For
      ;; right-associativity, the parser we got from @infixes should parse its
      ;; right argument greedily, so that there's nothing left for us to parse
      ;; here (at its infixity, anyway).
      (lambda (x) (p-infix-expr prec x)))))

(define p-expr (p-expr-at 0))

(define p-decl
  (>>= ask
    (lambda (parse-env)
      (try-one-maybe
        (lambda (t) (hash-lookup t (env-get @decls parse-env)))))
    (lambda (ext)
      ;; Pass off to its parser
      (@decl-parser ext))))

(define p-decls (many p-decl))

;; TODO: p-toplevel-decl p-toplevel-decls


;; -- Built-in parser parts --
;; - @exprs -
(define p-param p-id) ;; TODO: pattern parameters!
(define p-params (begin-sep-end-by p-param comma))
(define p-lambda (<$> expr:lambda (parens p-params) (<* p-expr p-optional-end)))
(define @expr:lambda (record [parser p-lambda]))

(define p-paren-expr (<* p-expr rparen))
(define @expr:parens (record [parser p-paren-expr]))

(define p-if-expr (<$> expr:if p-expr
                    (*> (keyword "then") p-expr)
                    (*> (keyword "else") p-expr)))
(define @expr:if (record [parser p-if-expr]))

(define p-let-expr (<$> expr:let (<* p-decls (keyword "in"))
                                 (<* p-expr p-optional-end)))
(define @expr:let (record [parser p-let-expr]))

;; - @infixes -
(define (p-seq-infix first-expr)
  (<$> (partial expr:seq first-expr) (p-expr-at 0)))
(define @infix:seq (record [precedence 0] [parser p-seq-infix]))

(define p-arguments (begin-sep-end-by p-expr comma))
(define (p-call-infix func-expr)
  (<$> (partial expr:call func-expr) (<* p-arguments rparen)))
(define @infix:call (record [precedence 11] [parser p-call-infix]))

;; - @decls -
(define p-val-decl
  ;; TODO: patterns!
  (<$> decl:val p-id (*> (keysym "=") p-expr)))
(define @decl:val (record [parser p-val-decl]))


;; -- The default/built-in parser extensions --
;; TODO: if, lambda, list, return, case, dict literals

(define builtin-@exprs
  (hash
    (TSYM "\\") @expr:lambda
    TLPAREN     @expr:parens
    (TID "if")  @expr:if
    ;; TODO: let
    (TID "let") @expr:let
    ))

(define builtin-@infixes
  (hash
    TLPAREN    @infix:call
    (TSYM ";") @infix:seq
    ;; TODO: arithmetic, (in)equalities, $, ||, &&, function composition
    ;; elm-style "|>" operator?
    ))

(define builtin-@decls
  (hash
    (TID "val") @decl:val
    ;; (TID "fun") @decl:fun
    ;; (TID "rec") @decl:rec
    ))

(define builtin-@pats (hash))

(define builtin-parse-env
  (hash
    @exprs builtin-@exprs
    @infixes builtin-@infixes
    @decls builtin-@decls
    @pats builtin-@pats
    ))

;; Precedence levels, taken from Haskell & modified slightly (not all of these
;; operators are actually in our language, but they could be):
;;
;;   infixR 0 ;
;;   infixR 1 $
;;   infixR 2 >> >>=
;;   infixL 3 <|>
;;   infixR 3 || &&& ***
;;   infixL 4 <$> <*> *> <*
;;   infixR 4 &&
;;   infix  5 == /= <= < >= >
;;   infixR 6 : ++
;;   infixL 7 + -
;;   infixL 8 * /
;;   infixR 9 ^
;;   infixR 10 .
;;   infixL 11 function application f(x,y,z)
;;
;; "IT GOES TO ELEVEN"
;;
;; TODO: a way to handle nonassociative infix operators (e.g. "a <= b <= c"
;; should result in a parse error)

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

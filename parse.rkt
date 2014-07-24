#lang racket

(require racket/stream)
(require (only-in parser-tools/lex position-token-token))

(require "util.rkt")
(require "values.rkt")
(require "env.rkt")
(require "lex.rkt")
(require "pcomb.rkt")
(require "core-forms.rkt")

(provide
  parse parse-all
  keyword keysym comma dot semi equals p-end p-optional-end
  lparen rparen lbrace rbrace lbrack rbrack
  parens braces brackets
  p-str p-num p-id
  listish
  ;; TODO: p-pat
  p-expr p-expr-at p-prefix-expr-at p-infix-expr
  p-decl p-decls
  parse-eval parse-eval-one)

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
(define equals (keysym "="))

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

;; A comma-separated-list, except leading and/or trailing commas are allowed.
(define (listish p) (begin-sep-end-by p comma))


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
          (lambda (t) (maybe-filter
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


;; This is it, folks. This is what it's all for.
;;
;; It's also weird (to me). It threads the evaluation through the parser rather
;; than repeatedly parsing and then evaluating.
(define (parse-eval resolve-env ns)
  ;; (printf "parse-eval: ~v\n" resolve-env) ;FIXME
  (let loop ([resolve-env resolve-env]
             [penv env-empty]
             [renv env-empty])
    (choice
      (>>= (parse-eval-one resolve-env ns)
        (lambda (result)
          ;; (printf "parse-eval: got result: ~v\n" result) ;; FIXME
          (let ([result-penv (result-parseExt result)]
                [result-renv (result-resolveExt result)])
            (local
              (lambda (parse-env) (env-join parse-env result-penv))
              (loop (env-join resolve-env result-renv)
                    (env-join penv result-penv)
                    (env-join renv result-renv))))))
      (eta (return (record [resolveExt renv] [parseExt penv]))))))

(define (parse-eval-one resolve-env ns)
  ;; (printf "parse-eval-one: ~v\n" resolve-env) ;FIXME
  (>>= ask
    (lambda (parse-env)
      (try-one-maybe
        (lambda (t)
          (match (hash-lookup t (env-get @tops parse-env))
            [(None) (maybe-map
                      (hash-lookup t (env-get @decls parse-env))
                      @top:@decl)]
            [x x]))))
    (lambda (ext)
      (@top-parse-eval ext resolve-env ns))))


;; This has to go here rather than builtin-parse.rkt since we use it in
;; parse-eval-one to handle regular decls in top-level position.
(define (@top:@decl decl)
  (record [parse-eval (parse-eval-decl (@decl-parser decl))]))

(define ((parse-eval-decl decl-parser) resolve-env ns)
  (>>= decl-parser
    (lambda (decl)
      (define code
        `(begin
           ,@(for/list ([id-code (decl-compile decl resolve-env)])
               `(define ,@id-code))))
      (printf "-- EVALING: ~v --\n" code) ;FIXME
      (eval code ns)
      (return (result:decl decl)))))

;; (result:decl Decl)
(define-form result:decl (decl)
  [resolveExt (decl-resolveExt decl)]
  [parseExt env-empty])

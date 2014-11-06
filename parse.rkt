#lang racket

(require racket/stream)
(require (only-in parser-tools/lex position-token-token))

(require "util.rkt")
(require "debug.rkt")
(require "values.rkt")
(require "objects.rkt")
(require "env.rkt")
(require "lex.rkt")
(require "pcomb.rkt")
(require "core-forms.rkt")
(require "engine.rkt")
(require (prefix-in q- "quasi.rkt"))

(provide
  parse parse-eval-file
  local-env
  keyword keysym comma dot semi colon equals bar
  lparen rparen lbrace rbrace lbrack rbrack
  parens braces brackets
  p-str p-num p-lit
  p-any-id p-id p-var-id p-caps-id
  p-qualified p-var
  listish
  p-expr p-expr-at p-prefix-expr p-atomic-expr p-infix-expr
  p-pat p-pat-at p-atomic-pat p-prefix-pat p-infix-pat
  p-decl p-decls
  parse-eval parse-eval-one)

(define (parse parser env what [whole #t])
  ((if whole (<* parser peof) parser)
    env
    (stream-stream
      (cond
        [(stream? what) what]
        [(port? what) (tokenize what)]
        [(string? what) (call-with-input-string what tokenize)]
        [#t (error 'parse "don't know how to parse: ~v" what)]))
    (lambda (loc msg) (error 'parse "hard error at pos ~a: ~a" loc msg))
    (lambda (loc msg) (error 'parse "soft error at pos ~a: ~a" loc msg))
    (lambda (_ r) r)))

;; Path, ParseEnv, ResolveEnv, Engine -> Result
(define (parse-eval-file filepath parse-env resolve-env eng)
  (call-with-input-file filepath
    (lambda (port)
      (debugf "Loading ~a...\n" filepath)
      (define x (parse (parse-eval resolve-env eng) parse-env port))
      (debugf "done!")
      x)))


;; Utility thingies
(define (local-env env p)
  (local (lambda (parse-env) (env-join parse-env env)) p))

(define (keyword id) (expect (TID id)))
(define (keysym id) (expect (TSYM id)))

(define comma (keysym ","))
(define dot (keysym "."))
(define semi (keysym ";"))
(define colon (keysym ":"))
(define equals (keysym "="))
(define bar (keysym "|"))

(define lparen (expect TLPAREN)) (define rparen (expect TRPAREN))
(define lbrace (expect TLBRACE)) (define rbrace (expect TRBRACE))
(define lbrack (expect TLBRACK)) (define rbrack (expect TRBRACK))

(define (parens x) (between lparen rparen x))
(define (braces x) (between lbrace rbrace x))
(define (brackets x) (between lbrack rbrack x))

(define p-str (<$> TSTR-value (satisfy TSTR?)))
(define p-num (<$> TNUM-value (satisfy TNUM?)))
(define p-lit (choice p-str p-num))

(define p-any-id (<$> (compose string->symbol TID-value) (satisfy TID?)))

(define rx-caps-ident #rx"^[A-Z]")
(define (capsname? s) (regexp-match? rx-caps-ident s))
(define (varname? s) (not (regexp-match? rx-caps-ident s)))

(define (p-id name ok?)
  (try-one-maybe
    (match-lambda [(TID s) #:when (ok? s) (Just (string->symbol s))]
             [_ None])
    (lambda (t) (format "expecting ~a, got ~v" name t))))

(define p-var-id (p-id "variable name" varname?))
(define p-caps-id (p-id "capitalized identifier" capsname?))

;; A comma-separated-list, except leading and/or trailing commas are allowed.
(define (listish p) (begin-sep-end-by p comma))


;; Module-qualified stuff

;; returns (path x)
;; where path is a list of symbols
;; and x is whatever id-parser returned
(define (p-qualified id-parser)
  (seq* (many (try (<* p-caps-id dot))) id-parser))

(define (qualify-var parse-env path name)
  (if (null? path) (var:local name)
    (match (resolve-nodule-path parse-env path)
      [(Ok nodule)
        ;; TODO: include entire path in var for better pretty-printed AST
        (var:qualified nodule (var:local name))]
      [(Err (list parse-env prefix suffix))
        ;; TODO: better error message that indicates which module wasn't bound
        (var:unbound (foldr cons name path))])))

(define (p-var id-parser)
  (pdo
    parse-env <- ask
    `(,path ,name) <- (p-qualified id-parser)
    (return (qualify-var parse-env path name))))


;; p-expr-at : Int -> Parse (Q Expr)
;;
;; Parses an expression at a given precedence (i.e. the longest expression that
;; contains no operators of looser precedence).
;;
;; Note on precedence: Larger precedences bind tighter than smaller, and
;; right-associative binds tighter than left-associative.
(define (p-expr-at prec)
  (pdo e <- (p-prefix-expr prec)
    (p-infix-expr prec e)))

;; This doesn't use `prec', but maybe in future it will?
(define (p-prefix-expr prec)
  (choice p-from-@exprs p-atomic-expr))

;; TODO: should atomic-expr be extensible?
(define p-atomic-expr
  (<$> q-pure
    (choice
      (<$> expr:lit p-lit)
      ;; an identifier.
      ;;
      ;; TODO: shouldn't we exclude identifiers which are used to trigger
      ;; parse-extensions from this?
      ;;
      ;; TODO: shouldn't only tag & variable identifiers be allowed, and e.g.
      ;; module identifiers be disallowed?
      (<$> expr:var (p-var p-any-id)))))

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
  (pdo
    parse-env <- ask ;; grab the extensible parsing environment
    ;; Grab a token and look it up in @exprs. Fail soft if it's absent.
    x <- (try-one-maybe (lambda (t) (hash-lookup t (env-get @exprs parse-env))))
    ;; Run the parser we found in @exprs!
    x))

;; Tries to parse an infix continuation for `left-expr' of precedence at least
;; `prec' (i.e. binding at least as tightly as `prec').
(define (p-infix-expr prec left-expr)
  ;; TODO: should we have a separate @infix-ops?
  (option left-expr
    (pdo
      parse-env <- ask
      ext <- (try-one-maybe
               ;; Look for an infix operator associated with the token `t' whose
               ;; precedence is at least `prec' (i.e. as tight or
               ;; tighter-binding as what we're currently looking for).
               (lambda (t) (maybe-filter
                        (hash-lookup t (env-get @infix-exprs parse-env))
                        (lambda (x) (<= prec (@infix-expr-precedence x))))))
      ;; Pass off to their parser
      x <- (@infix-expr-parse ext left-expr)
      ;; Try to keep parsing more operations afterward.
      ;;
      ;; Note: This handles left-associativity automatically. For
      ;; right-associativity, the parser we got from @infixes should parse its
      ;; right argument greedily, so that there's nothing left for us to parse
      ;; here (at its infixity, anyway).
      (p-infix-expr prec x))))

(define p-expr (p-expr-at 0))

;; p-pat-at : Parse (Q Pat)
;; Parses a pattern at a given precedence.
(define (p-pat-at prec)
  (pdo e <- (p-prefix-pat prec)
    (p-infix-pat prec e)))

(define (p-prefix-pat prec)
  (choice
    p-from-@pats
    p-atomic-pat
    ;; TODO: this tag/ann-matching behavior shouldn't be hard-coded in! :(
    ;; TODO: tag/ann patterns without parens afterwards, e.g. Nil
    (<$>
      (lambda (id args) (q-fmap (curry pat:tagged id) (q-seq args)))
      (p-var p-caps-id)
      ;; eta necessary to avoid circularity
      (option '() (eta (parens (listish p-pat)))))))

(define p-atomic-pat
  ;; TODO: underscore pattern?
  (<$> q-pure (choice (<$> pat:lit p-lit) (<$> pat:var p-var-id))))

(define p-from-@pats
  (pdo
    parse-env <- ask ;; grab the extensible parsing environment
    ;; Grab a token and look it up in @pats. Fail soft if it's absent.
    x <- (try-one-maybe (lambda (t) (hash-lookup t (env-get @pats parse-env))))
    ;; Run the parser we found in @pats!
    x))

(define (p-infix-pat prec left-pat)
  (option left-pat
    (pdo
      parse-env <- ask
      ext <- (try-one-maybe
               ;; Look for an infix pattern associated with token `t' whose
               ;; precedence is at least `prec'.
               (lambda (t) (maybe-filter
                        (hash-lookup t (env-get @infix-pats parse-env))
                        (lambda (x) (<= prec (@infix-pat-precedence x))))))
      x <- (@infix-pat-parse ext left-pat)
      ;; Try to keep parsing more operations afterward.
      (p-infix-pat prec x))))

(define p-pat (p-pat-at 0))

;; p-decl : Parse (ParseEnv, Q Decl)
(define p-decl
  (pdo
    parse-env <- ask
    ext <- (try-one-maybe
             (lambda (t) (hash-lookup t (env-get @decls parse-env))))
    ;; Pass off to its parser
    ext))

;; p-decls : Parse (ParseEnv, Q [Decl])
;; TODO: unquote-splicing in p-decls?
(define p-decls
  (let recur ([es env-empty]
              [ds '()])
    (choice
      (pdo `(,e ,d) <- p-decl
        (local-env e
          (recur (env-join es e) (cons d ds))))
      (eta (return (list es (q-seq (reverse ds))))))))


;; This is it, folks. This is what it's all for.
;;
;; It also feels weird (to me). It threads the evaluation through the parser
;; rather than repeatedly parsing and then evaluating.
;;
;; ResolvEnv, Engine -> Parser Result
(define (parse-eval resolve-env eng)
  (let loop ([resolve-env resolve-env]
             [penv env-empty]
             [renv env-empty])
    (choice
      (pdo result <- (parse-eval-one resolve-env eng)
        (let ([result-penv (result-parseExt result)]
              [result-renv (result-resolveExt result)])
          (local-env result-penv
            (loop (env-join resolve-env result-renv)
              (env-join penv result-penv)
              (env-join renv result-renv)))))
      (eta (return (record [resolveExt renv] [parseExt penv]))))))

;; ResolvEnv, Engine -> Parser Result
(define (parse-eval-one resolve-env eng)
  (pdo
    parse-env <- ask
    ext <- (try-one-maybe
             (lambda (t)
               (match (hash-lookup t (env-get @tops parse-env))
                 [(None) (maybe-map
                           (hash-lookup t (env-get @decls parse-env))
                           @top:@decl)]
                 [x x])))
    (ext resolve-env eng)))


;; This has to go here rather than parse-builtins.rkt since we use it in
;; parse-eval-one to handle regular decls in top-level position.
(define ((@top:@decl decl-parser) resolve-env eng)
  (>>= decl-parser
    (lambda (d)
      (match-define `(,decl-env ,decl-q) d)
      (define decl (q-run decl-q))
      (debugf-pretty " * AST:" (decl-sexp decl))
      (define code
        `(begin
           ,@(for/list ([id-code (decl-compile decl resolve-env)])
               `(define ,@id-code))))
      (debugf-pretty " * IR:" code)
      (eval code (engine-namespace eng))
      (return (result:decl decl-env decl)))))

;; (result:decl Decl)
(define-result decl (env decl)
  [resolveExt (decl-resolveExt decl)]
  [parseExt env])

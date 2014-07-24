#lang racket

(require "util.rkt")
(require "values.rkt")
(require "env.rkt")
(require "lex.rkt")
(require "pcomb.rkt")
(require "parse.rkt")

;; Utilities
(define p-params (listish p-id))


;; -- decls --
(provide decl:val decl:fun decl:rec decl:tag)

;; (val Symbol Expr)
(define-form decl:val (name expr)
  [(sexp) `(val ,name ,(expr-sexp expr))]
  [id (gensym name)]
  [resolveExt (env-single @vars (@vars-var name id))]
  [(compile env) `((,id ,(expr-compile expr env)))])

(define @decl:val
  ;; TODO: patterns, rather than just identifiers!
  (record [parser (<$> decl:val p-id (*> equals p-expr))]))

;; (fun name:Symbol params:[Symbol] body:Expr)
;; TODO: branches, patterns
(define-form decl:fun (name params expr)
  [(sexp) `(fun ,name ,params ,(expr-sexp expr))]
  [id (gensym name)]
  [resolveExt (env-single @vars (@vars-var name id))]
  [(compile env)
    (let ([inner-env (env-join env resolveExt)])
      `((,id ,(expr-compile (expr:lambda params expr) inner-env))))])

(define @decl:fun
  (record [parser (<$> decl:fun p-id (parens p-params) (*> equals p-expr))]))

;; (rec [Decl])
(define-form decl:rec (decls)
  [(sexp) `(rec ,@(map decl-sexp decls))]
  [resolveExt (env-join* (map decl-resolveExt decls))]
  ;; TODO: we should check for definition cycles somehow!
  ;; if we compiled to IR instead of to Racket this might be easier.
  [(compile env)
    (let ([env (env-join env resolveExt)])
      (apply append (map (lambda (x) (decl-compile x env)) decls)))])

(define @decl:rec
  (record [parser (<$> decl:rec (sep-by1 p-decl (keyword "and")))]))

;; (tag name:Symbol params:(Maybe [Symbol]))
;; TODO: require tags be upper-case?
;; TODO: require other identifiers be lower-case?
(define-form decl:tag (name params)
  [(sexp) `(tag ,name ,params)]
  [id (gensym (format "ctor:~a" name))]
  [tag-id (gensym (format "tag:~a" name))]
  [info (@var:ctor name id tag-id (maybe params 0 length))]
  [resolveExt (env-single @vars (hash name info))]
  [(compile env)
    `((,tag-id (new-tag ',name ',(from-maybe params '())))
      (,id ,(match params
              [(Just l) `(lambda ,l (make-ann ,tag-id ,@l))]
              [(None) `(make-ann ,tag-id)])))])

(define @decl:tag
  (record [parser (<$> decl:tag p-id (option-maybe (parens (listish p-id))))]))

(define builtin-@decls
  (hash
    (TID "val") @decl:val
    (TID "fun") @decl:fun
    (TID "tag") @decl:tag
    (TID "rec") @decl:rec
    ))


;; -- exprs --
(provide expr:lambda expr:let expr:if)

(define @expr:parens (record [parser (<* p-expr rparen)]))

;; (lambda params:[Symbol] body:Expr)
;; TODO: pattern parameters. case-lambdas? rec-lambdas?
(define-form expr:lambda (params expr)
  ;; TODO: case-lambdas? patterns as parameters?
  ;; TODO: check for duplicate names in params
  [(sexp) `(lambda ,params ,(expr-sexp expr))]
  [(compile env)
    (let* ([ids (map gensym params)]
           [vars (hash-from-keys-values
                   params
                   (map @var:var params ids))]
           [inner-env (env-join env (env-single @vars vars))])
      `(lambda (,@ids)
         ,(expr-compile expr inner-env)))])

(define @expr:lambda
  (record [parser (<$> expr:lambda
                    (parens p-params)
                    (<* p-expr p-optional-end))]))

;; (let [Decl] Expr)
(define-form expr:let (decls exp)
  [(sexp) `(let ,(map decl-sexp decls) ,(expr-sexp exp))]
  [(compile env)
    (let loop ([decls decls] [env env])
      (match decls
        ['() (expr-compile exp env)]
        [(cons d ds)
          `(letrec ,(decl-compile d env)
             ,(loop ds (env-join env (decl-resolveExt d))))]))])

(define @expr:let
  (record [parser (<$> expr:let
                    (<* p-decls (keyword "in"))
                    (<* p-expr p-optional-end))]))

;; (if Expr Expr Expr)
(define-form expr:if (subject then else)
  [(sexp) `(if ,(expr-sexp subject) ,(expr-sexp then) ,(expr-sexp else))]
  [(compile env)
    `(if ,(expr-compile subject env)
       ,(expr-compile then env)
       ,(expr-compile else env))])

(define @expr:if
  (record [parser (<$> expr:if p-expr
                    (*> (keyword "then") p-expr)
                    (*> (keyword "else") p-expr))]))

(define builtin-@exprs
  (hash
    TLPAREN     @expr:parens
    (TSYM "\\") @expr:lambda
    (TID "let") @expr:let
    (TID "if")  @expr:if
    ))


;; -- infixes --
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

(provide expr:call expr:seq)

;; (call Expr [Expr])
(define-form expr:call (func args)
  [(sexp) (map expr-sexp (cons func args))]
  [(compile env) (map (lambda (x) (expr-compile x env)) (cons func args))])

(define @infix:call
  (record
    [precedence 11]
    [(parser func-expr) (<$> (partial expr:call func-expr)
                             (<* (listish p-expr) rparen))]))

;; (seq Expr Expr)
(define-form expr:seq (a b)
  [(sexp) `(begin ,(expr-sexp a) ,(expr-sexp b))]
  [(compile env) `(begin ,(expr-compile a env) ,(expr-compile b env))])

(define @infix:seq
  (record
    [precedence 0]
    [(parser first-expr) (<$> (partial expr:seq first-expr) (p-expr-at 0))]))

(define builtin-@infixes
  (hash
    TLPAREN    @infix:call
    (TSYM ";") @infix:seq
    ;; TODO: arithmetic, (in)equalities, $, ||, &&, function composition
    ;; elm-style "|>" operator?
    ))


;; -- pats --
;; TODO: expose these in parse env
(provide pat:one pat:zero pat:let pat:var pat:ann pat:vector)

(define-form pat:one () ;; "underscore" pattern, _, succeeds binding nothing
  [resolveExt env-empty]
  [(compile env subject on-success on-failure) on-success])

(define-form pat:zero (names) ;; pattern that always fails, binding names
  [ids (map gensym names)]
  [resolveExt (env-single @vars (hash-from-keys-values names
                                  (map @var:var names ids)))]
  [(compile env subject on-success on-failure) on-failure])

(define-form pat:let (name expr) ;; always binds name to expr
  [id (gensym name)]
  [resolveExt (env-single @vars (@vars-var name id))]
  [(compile env subject on-success on-failure)
    `(let ((,id ,(expr-compile expr env))) ,on-success)])

(define-form pat:var (name)
  [id (gensym name)]
  [resolveExt (env-single @vars (@vars-var name id))]
  [(compile env subject on-success on-failure)
    `(let ([,id ,subject]) ,on-success)])

(define-form pat:ann (name args)
  [args-pat (pat:vector args)]
  [resolveExt (pat-resolveExt args-pat)]
  [(compile env subject on-success on-failure)
    ;; TODO: useful error handling.
    (let* ([info (hash-get name (env-get @vars env)
                   (lambda () (error 'pat:ann "unbound ctor ~v" name)))]
           [ctor-id (@var-id info)]
           [tag-id (hash-get 'tag-id info
                     (lambda () (error 'pat:ann "~v is not a ctor" name)))]
           [tag-arity (hash-get 'tag-arity info)]
           [tmp (gensym 'tmp)])
      `(if (ann-isa? ,tag-id ,subject)
         ;; match each pat in args against (ann-args subject)
         (let ((,tmp (ann-args ,subject)))
           ,(pat-compile args-pat env tmp on-success on-failure))
         ,on-failure))])

(define-form pat:vector (elems)
  [resolveExt (env-join* (map pat-resolveExt elems))]
  [(compile env subject on-success on-failure)
    (let loop ([i 0] [env env])
      (if (>= i (vector-length elems)) on-success
        (let ([tmp (gensym 'tmp)]
              [elem (vector-ref elems i)])
          `(let ([,tmp (vector-ref ,subject ',i)])
             ,(pat-compile elem tmp
                (loop (+ i 1) (env-join env (pat-resolveExt elem)))
                on-failure)))))])

;; TODO: pat:and, pat:or, pat:lit, pat:guard

(define builtin-@pats (hash))


;; -- tops & their results --
;; TODO: import, module, define-extension-point, define-extension
(provide result:empty result:import)

(define-form result:empty ()
  [resolveExt env-empty]
  [parseExt env-empty])

;; TODO: more powerful imports (qualifying, renaming, etc.)
;; (import Nodule)
(define-form result:import (nodule)
  [resolveExt (nodule-resolveExt nodule)]
  [parseExt (nodule-parseExt nodule)])

(define builtin-@tops (hash))


;; -- The default/built-in parser extensions --
;; TODO: list, case, dict literals
(provide builtin-parse-env)

(define builtin-parse-env
  (hash
    @exprs builtin-@exprs
    @infixes builtin-@infixes
    @decls builtin-@decls
    @pats builtin-@pats
    @tops builtin-@tops))


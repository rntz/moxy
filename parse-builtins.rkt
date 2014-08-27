#lang racket

(require "debug.rkt")
(require "util.rkt")
(require "values.rkt")
(require "objects.rkt")
(require "env.rkt")
(require "lex.rkt")
(require "pcomb.rkt")
(require "parse.rkt")
(require "core-forms.rkt")
(require "runtime.rkt")                 ;for engine-builtin-resolve-env

;; Utilities
(define p-var-ids (listish p-var-id))
(define p-params (listish p-pat))

;; ir-case: ResolveEnv, IR, IR, [(Pat, Expr)] -> IR
;; assumes subject, on-failure are small
(define (ir-case env subject on-failure branches)
  (ir-multicase env (list subject) on-failure
    (map (match-lambda [`(,pat ,expr) `((,pat) ,expr)]) branches)))

;; ResolveEnv, [IR], IR, [([Pat], Expr)] -> IR
(define (ir-multicase env subjects on-failure branches)
  ;; if this foldr were changed to a foldl, we would try the last pattern first
  ;; instead. so don't do that.
  (foldr
    (lambda (branch on-failure)
      (match-let ([`(,pats ,expr) branch])
        (let ((fail-id (mktemp 'fail)))
          `(let ((,fail-id (lambda () ,on-failure)))
             ,(ir-match-each env (zip pats subjects) expr `(,fail-id))))))
    on-failure
    branches))

;; ResolveEnv, [(Pat, IR)], Expr, IR -> IR
(define (ir-match-each env pat-subjs on-success-expr on-failure-ir)
  (match pat-subjs
    ['() (expr-compile on-success-expr env)]
    [`((,pat ,subj) . ,rest-pat-subjs)
      (pat-compile pat env subj
        (ir-match-each (env-join env (pat-resolveExt pat))
          rest-pat-subjs on-success-expr on-failure-ir)
        on-failure-ir)]))


;; -- decls --
(provide decl:val decl:fun decl:rec decl:tag)

;; (val Pat Expr)
(define-decl val (pat expr)
  [(sexp) `(val ,(pat-sexp pat) ,(expr-sexp expr))]
  [parseExt env-empty]
  [resolveExt (pat-resolveExt pat)]
  [(compile env)
    ;; in this case, we don't need an intermediate vector.
    (let* ([rhs-tmp (mktemp 'rhs)]
           [on-failure `(error 'decl:val
                          "In ~a: pattern ~a did not match value: ~v"
                          ',(show (sexp))
                          ',(show (pat-sexp pat))
                          ,rhs-tmp)])
      (cons `(,rhs-tmp ,(expr-compile expr env))
        (match (pat-idents pat)
          ['()
            `((,(mktemp 'ignored)
               ,(pat-compile pat env rhs-tmp `',(void) on-failure)))]
          [`(,id)
            `((,id ,(pat-compile pat env rhs-tmp id on-failure)))]
          [idents
            ;; in the general case, we need to allocate an intermediate vector
            ;; of results from the pattern-match. :(
            (let ((vector-tmp (mktemp 'vector)))
              `((,vector-tmp ,(pat-compile pat env rhs-tmp `(vector ,@idents)
                                on-failure))
                ,@(for/list ([(id i) (in-indexed idents)])
                    `(,id (vector-ref ,vector-tmp ',i)))))])))])

(define @decl:val
  (record [parser (<$> decl:val p-pat (*> equals p-expr))]))

;; (fun name:Symbol arity:Nat branches:[Branch])
;; where Branch = (params:[Pat], body:Expr)
;; where each params-list is of length `arity'
(define-decl fun (name arity branches)
  [(sexp) `(fun ,name ,@(for/list ([b branches])
                          (match-let ([`(,params ,body) b])
                            `(,(map pat-sexp params) ,(expr-sexp body)))))]
  [id (mkid name)]
  [parseExt env-empty]
  [resolveExt (env-single @vars (@vars-var name id))]
  [func-expr (expr:case-lambda arity branches
               (expr:racket `(error "Non-exhaustive cases in fun!")))]
  [(compile env)
    (let ([inner-env (env-join env resolveExt)])
      `((,id ,(expr-compile func-expr inner-env))))])

(define p-fun-clause (seq* p-var-id (parens p-params) (*> equals p-expr)))
(define p-fun
  (pdo clauses <- (begin-sep-by1 p-fun-clause bar)
    (let/ec escape
      (match-define `((,name ,params ,_) . ,_) clauses)
      (define arity (length params))
      (define case-branches
        (for/list ([clause clauses])
          (match-define `(,clause-name ,clause-params ,clause-body) clause)
          (unless (symbol=? name clause-name)
            (escape (fail "fun clauses have varying names")))
          (unless (= arity (length clause-params))
            (escape (fail "fun clauses have varying arity")))
          (list clause-params clause-body)))
      (return (decl:fun name arity case-branches)))))

(define @decl:fun
  (record [parser p-fun]))

;; (rec [Decl])
(define-decl rec (decls)
  [(sexp) `(rec ,@(map decl-sexp decls))]
  [parseExt env-empty]
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
(define-decl tag (name params)
  [(sexp) `(tag ,name ,params)]
  [id (mkid "ctor:~a" name)]
  [tag-id (mkid "tag:~a" name)]
  [info (@var:ctor name id tag-id params)]
  [parseExt env-empty]
  [resolveExt (env-single @vars (hash name info))]
  [(compile env)
    `((,tag-id (new-tag ',name ',(from-maybe params '())))
      (,id ,(match params
              [(Just l) `(lambda ,l (make-ann ,tag-id ,@l))]
              [(None) `(make-ann ,tag-id)])))])

(define @decl:tag
  (record [parser (<$> decl:tag p-caps-id
                    (option-maybe (parens (listish p-var-id))))]))

;; TODO: more powerful imports (qualifying, renaming, etc.)
;; (open Nodule)
(define-decl open (name nodule)
  [(sexp) `(open ,name)]
  [resolveExt (@nodule-resolveExt nodule)]
  [parseExt (@nodule-parseExt nodule)]
  [(compile env) '()])

(define @decl:open
  (record [parser
            (pdo parse-env <- ask
              (<$> (unary decl:open)
                (pmap-maybe p-caps-id
                  (lambda (name)
                    (maybe-map (hash-lookup name (env-get @nodules parse-env))
                      (lambda (x) `(,name ,x))))
                  (lambda (name)
                    (format "expected module name; got ~v" name)))))]))

(define builtin-@decls
  (hash
    (TID "val")     @decl:val
    (TID "fun")     @decl:fun
    (TID "tag")     @decl:tag
    (TID "rec")     @decl:rec
    (TID "open")    @decl:open))


;; -- exprs --
(provide expr:lambda expr:let expr:if expr:case)

(define @expr:parens (record [parser (<* p-expr rparen)]))

;; (case-lambda arity:Nat branches:[Branch] on-failure:Expr)
;; where Branch = (params:[Pat], body:Expr)
;; where each `params' is of length `arity'
(define-expr case-lambda (arity branches on-failure)
  [(sexp) `(case-lambda
             ,(for/list ([b branches])
                (match-let ([`(,pats ,body) b])
                  `(,(map pat-sexp pats) ,(expr-sexp body))))
              ,(expr-sexp on-failure))]
  [(compile env)
    ;; TODO?: make gensyms be based on the actual argument names if possible?
    (let* ([arg-ids (for/list ([i (in-range arity)])
                      (mktemp "arg:~a" i))])
      `(lambda ,arg-ids
         ,(ir-multicase env arg-ids (expr-compile on-failure env) branches)))])

;; (lambda params:[Pat] body:Expr)
;; thin wrapper around case-lambda
(define (expr:lambda params body)
  (expr:case-lambda (length params)
    (list (list params body))
    (expr:racket `(error "lambda parameter pattern did not match"))))

;; for now, lambdas don't get multiple branches, but you can pattern-match.
(define @expr:lambda
  (record [parser (<$> expr:lambda
                    (parens p-params)
                    (<* p-expr p-optional-end))]))

;; (let [Decl] Expr)
(define-expr let (decls exp)
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
(define-expr if (subject then else)
  [(sexp) `(if ,(expr-sexp subject) ,(expr-sexp then) ,(expr-sexp else))]
  [(compile env)
    `(if (truthy? ,(expr-compile subject env))
       ,(expr-compile then env)
       ,(expr-compile else env))])

(define @expr:if
  (record [parser (<$> expr:if p-expr
                    (*> (keyword "then") p-expr)
                    (*> (keyword "else") p-expr))]))

;; (case Expr [(Pat, Expr)])
(define-expr case (subject branches)
  [(sexp) `(case ,(expr-sexp subject)
             ,@(for/list ([b branches])
                 (match-let ([`(,pat ,body) b])
                   `(,(pat-sexp pat) ,(expr-sexp body)))))]
  [subject-id (gensym 'case-subject)]
  [(compile env)
    ;; TODO: check whether (expr-compile subject env) is "small" and, if so,
    ;; omit binding it to subject-id.
    `(let ([,subject-id ,(expr-compile subject env)])
       ,(ir-case env subject-id `(error "Non-exhaustive patterns in case!")
          branches))])

(define @expr:case
  ;; TODO: shouldn't we be parsing the subject at a certain precedence?
  ;; consider e.g. (case (case x ...) ...)
  (record [parser (<$> expr:case
                    p-expr
                    (<* (many (*> bar (seq* p-pat (*> (keysym "->") p-expr))))
                        p-optional-end))]))

(define builtin-@exprs
  (hash
    TLPAREN      @expr:parens
    (TSYM "\\")  @expr:lambda
    (TID "let")  @expr:let
    (TID "if")   @expr:if
    (TID "case") @expr:case
    ))


;; -- infixes --
(provide expr:call expr:seq)

;; (call Expr [Expr])
(define-expr call (func args)
  [(sexp) (map expr-sexp (cons func args))]
  [(compile env) (map (lambda (x) (expr-compile x env)) (cons func args))])

(define @infix:call
  (record
    [precedence 11]
    [(parser func-expr) (<$> (partial expr:call func-expr)
                             (<* (listish p-expr) rparen))]))

;; (seq Expr Expr)
(define-expr seq (a b)
  [(sexp) `(begin ,(expr-sexp a) ,(expr-sexp b))]
  [(compile env) `(begin ,(expr-compile a env) ,(expr-compile b env))])

(define @infix:seq
  (record
    [precedence 0]
    [(parser first-expr) (<$> (partial expr:seq first-expr) (p-expr-at 0))]))

;; infix operators
;; associativity must be Left or Right.
(define-@infix oper (assoc precedence function)
  [(parser left-arg)
    (<$>
      (lambda (right-arg) (expr:call (expr:lit function)
                                (list left-arg right-arg)))
      (p-expr-at (match assoc
                   [(L) (+ precedence 1)]
                   [(R) precedence])))])

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
;; TODO: nonassociative infix operators (e.g. "a <= b <= c" should result in a
;; parse error)
;;
;; TODO: Backticks to make `infix` operators.
;; TODO: string append operator.
;; TODO: prefix-ifying an infix operator / slicing syntax.

(define builtin-@infixes
  (hash
    TLPAREN    @infix:call
    (TSYM ";") @infix:seq

    ;; TODO: ||, &&, function composition, exponentiation, parser-combinators
    ;; elm-style "|>" operator?

    ;; TODO: make (in)equality operators nonassociative
    ;; TODO: generic inequality operators
    (TSYM "==") (@infix:oper L 5 (compose truthify equal?))
    (TSYM "<")  (@infix:oper L 5 (compose truthify <))
    (TSYM ">")  (@infix:oper L 5 (compose truthify >))
    (TSYM "<=") (@infix:oper L 5 (compose truthify <=))
    (TSYM ">=") (@infix:oper L 5 (compose truthify >=))

    (TSYM "+")  (@infix:oper L 7 +)
    (TSYM "-")  (@infix:oper L 7 -)
    (TSYM "*")  (@infix:oper L 8 *)
    (TSYM "/")  (@infix:oper L 8 /)
    ))


;; -- pats --
;; TODO: expose these in parse env
(provide pat:one pat:zero pat:let)

(define-pat one () ;; "underscore" pattern, _, succeeds binding nothing
  [(sexp) '_]
  [resolveExt env-empty]
  [idents '()]
  [(compile env subject on-success on-failure) on-success])

(define-pat zero (names) ;; pattern that always fails, binding `names'
  [(sexp) `(zero ,@names)]
  [idents (map gensym names)]
  [resolveExt (env-single @vars (hash-from-keys-values names
                                  (map @var:var names idents)))]
  [(compile env subject on-success on-failure) on-failure])

(define-pat let (name expr) ;; always binds name to expr
  [(sexp) `(let ,name ,(expr-sexp expr))]
  [id (gensym name)]
  [resolveExt (env-single @vars (@vars-var name id))]
  [idents (list id)]
  [(compile env subject on-success on-failure)
    `(let ((,id ,(expr-compile expr env))) ,on-success)])

;; TODO: pat:and, pat:or, pat:guard

(define builtin-@pats (hash))


;; -- infix patterns --
(provide)

(define builtin-@infix-pats
  (hash))


;; -- tops & their results --
;; TODO: import, module, define-extension-point, define-extension
(provide result:empty result:nodule)

(define-result empty ()
  [resolveExt env-empty]
  [parseExt env-empty])

(define-result nodule (name result)
  [resolveExt env-empty]
  [parseExt (env-single @nodules (@nodules-nodule name result))])

;; 'module' ID '{' TOPS '}'
(define @top:nodule
  (record
    [(parse-eval resolve-env eng)
     (<$> result:nodule
       p-caps-id (braces (parse-eval resolve-env eng)))]))

;; import Name
(define @top:import
  (record
    [(parse-eval resolve-env eng)
      (>>= p-caps-id
        (lambda (name)
          (define result
            (parse-eval-file
              (format "~a.cvy" (string-downcase (symbol->string name)))
              builtin-parse-env
              (engine-builtin-resolve-env eng)
              eng))
          (return (result:nodule name result))))]))

(define builtin-@tops
  (hash
    (TID "module")  @top:nodule
    (TID "import")  @top:import))


;; -- The default/built-in parser extensions --
;; TODO: list, case, dict literals
(provide builtin-parse-env)

(define builtin-parse-env
  (hash
    @decls builtin-@decls
    @exprs builtin-@exprs
    @infixes builtin-@infixes
    @pats builtin-@pats
    @infix-pats builtin-@infix-pats
    @tops builtin-@tops))


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
(require "engine.rkt")
(require (prefix-in q- "quasi.rkt"))

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


;; -- quote forms --
(define builtin-@quote-forms
  (hash
    (TID "e") p-expr
    ;; cadr is needed to grab the (Q Decl) from the (ParseEnv, Q Decl)
    ;; TODO: should there be some way of getting the ParseEnv, too?
    (TID "d") (<$> cadr p-decl)
    (TID "p") p-pat
    ;; TODO?: more quasiquotation types (top?)
    ))

;; should these really go here?
(provide p-unquo-expr q-ify q-listish)

(define p-unquo-expr (<$> q-unquo (choice p-atomic-expr (parens p-expr))))

;; q-listish : Parse (Q a) -> Parse (Q [a])
;; handles unquote-splicing
(define ((q-ify listish) p)
  (define elem (choice (*> (keysym "~..") p-unquo-expr) (<$> q-seq* p)))
  (<$> (compose (q-lift append*) q-seq) (listish elem)))

(define q-listish (q-ify listish))

;; p-pats : Parse (Q [Pat])
(define p-pats (q-listish p-pat))


;; -- decls --
(provide decl:val decl:fun decl:begin decl:rec decl:tag)

;; (val Pat Expr)
(define-decl val (pat expr)
  [(sexp) `(val ,(pat-sexp pat) ,(expr-sexp expr))]
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

;; make-@val : Q Pat, Q Expr -> (ParseEnv, Q Expr)
(define (make-@val pat expr) (list env-empty (q-fmap decl:val pat expr)))
(define @decl:val (<$> make-@val p-pat (*> equals p-expr)))

;; (fun name:Symbol arity:Nat branches:[Branch])
;; where Branch = (params:[Pat], body:Expr)
;; where each `params'-list is of length `arity'
(define-decl fun (name arity branches)
  [(sexp) `(fun ,name ,@(for/list ([b branches])
                          (match-let ([`(,params ,body) b])
                            `(,(map pat-sexp params) ,(expr-sexp body)))))]
  [id (mkid name)]
  [resolveExt (env-single @vars (@vars-var name id))]
  [func-expr (expr:case-lambda arity branches
               (expr:racket `(error "Non-exhaustive cases in fun!")))]
  [(compile env)
    (let ([inner-env (env-join env resolveExt)])
      `((,id ,(expr-compile func-expr inner-env))))])

;; make-fun : [(Symbol, [Pat], Expr)] -> Expr
(define (make-fun clauses)
  (match-define `((,name ,params ,_) . ,_) clauses)
  (define arity (length params))
  (define case-branches
    (for/list ([clause clauses])
      (match-define `(,clause-name ,clause-params ,clause-body) clause)
      (unless (symbol=? name clause-name)
        (error "fun clauses have varying names"))
      (unless (= arity (length clause-params))
        (error "fun clauses have varying arity"))
      (list clause-params clause-body)))
  (decl:fun name arity case-branches))

;; p-fun-clause : Parse (Q (Symbol, [Pat], Expr))
;; p-fun : Parse (Q Decl)
(define p-fun-clause
  (<$> q-seq (seq* (<$> q-pure p-var-id) (parens p-pats) (*> equals p-expr))))
(define p-fun (<$> (compose (q-lift make-fun) q-seq)
                (begin-sep-by1 p-fun-clause bar)))
(define @decl:fun (<$> (partial list env-empty) p-fun))

;; (begin [Decl])
;; expr:let uses this
(define-decl begin (decls)
  [(sexp) `(,@(map decl-sexp decls))]
  [resolveExt (env-join* (map decl-resolveExt decls))]
  [(compile env)
    (let loop ([decls decls]
               [accum '()]
               [env env])
      (match decls
        ['() (append* (reverse accum))]
        [(cons d ds)
          (loop ds (cons (decl-compile d env) accum)
            (env-join env (decl-resolveExt d)))]))])

(define p-decl-group
  (<$> (match-lambda [`(,env ,ds) `(,env ,(q-fmap decl:begin ds))]) p-decls))

(define @decl:begin (<* p-decl-group (keyword "end")))

;; (rec [Decl])
(define-decl rec (decls)
  [(sexp) `(rec ,@(map decl-sexp decls))]
  [resolveExt (env-join* (map decl-resolveExt decls))]
  ;; TODO: we should check for definition cycles somehow!
  ;; if we compiled to IR instead of to Racket this might be easier.
  [(compile env)
    (let ([env (env-join env resolveExt)])
      (apply append (map (lambda (x) (decl-compile x env)) decls)))])

;; make-decl:rec : [(ParseEnv, Q Decl)] -> (ParseEnv, Q Decl)
(define (make-@rec decls)
  (list (env-join* (map car decls))
    (q-fmap decl:rec (q-seq (map cadr decls)))))

(define @decl:rec
  ;; NB. this doesn't allow later decls to see earlier decl's parse extensions.
  ;; TODO: is this the right behavior?
  ;;
  ;; really, what's going on is that when parsing declarations I want a (State
  ;; ParseEnv); and when parsing other things I want a (Reader ParseEnv) monad.
  (<$> make-@rec (sep-by1 p-decl (keyword "and"))))

;; (tag name:Symbol params:(Maybe [Symbol]))
;; TODO: require tags be upper-case?
;; TODO: require other identifiers be lower-case?
(define-decl tag (name params)
  [(sexp) `(tag ,name ,params)]
  [id (mkid "ctor:~a" name)]
  [tag-id (mkid "tag:~a" name)]
  [info (@var:ctor name id tag-id params)]
  [resolveExt (env-single @vars (hash name info))]
  [(compile env)
    `((,tag-id (new-tag ',name ',(from-maybe params '())))
      (,id ,(match params
              [(Just l) `(lambda ,l (make-ann ,tag-id ,@l))]
              [(None) `(make-ann ,tag-id)])))])

(define (make-@decl i ps) `(,env-empty ,(q-pure (decl:tag i ps))))
(define @decl:tag
  (<$> make-@decl p-caps-id (option-maybe (parens (listish p-var-id)))))

;; (local Decl Decl)
(define-decl local (decl body)
  [(sexp) `(local ,(decl-sexp decl) ,(decl-sexp body))]
  [resolveExt (decl-resolveExt body)]
  [(compile env)
    (append
      (decl-compile decl env)
      (decl-compile body (env-join env (decl-resolveExt decl))))])

(define @decl:local
  (pdo
    `(,env1 ,decl1) <- p-decl-group
    (keyword "in")
    `(,env2 ,decl2) <- (local-env env1 p-decl-group)
    (keyword "end")
    (return `(,env2 ,(q-fmap decl:local decl1 decl2)))))

;; TODO: more powerful imports (qualifying, renaming, etc.)
(define-decl open (path nodule)
  [(sexp) `(open ,path)]
  [resolveExt (@nodule-resolveExt nodule)]
  [(compile env) '()])

(define @decl:open
  (pdo
    parse-env <- ask
    `(,path ,name) <- (p-qualified p-caps-id)
    let path (append path (list name))
    (match (resolve-nodule-path parse-env path)
      [(Ok nodule) (return `(,(@nodule-parseExt nodule)
                             ,(q-pure (decl:open path nodule))))]
      [(Err _)
        ;; TODO: better error message using information from Err
        (fail (format "unbound module: ~a" path))])))

;; extension name(identity, operator)
(define-decl extension (name empty join)
  [(sexp) `(extension ,name ,(expr-sexp empty) ,(expr-sexp join))]
  [id (mkid name)]
  [resolveExt (env-single @vars (@vars-var name id))]
  [(compile env)
    `((,id (,make-ExtPoint ',name
             ,(expr-compile join env)
             ,(expr-compile empty env))))])

(define (make-@extension name empty-join)
  (match-define `(,empty ,join) empty-join)
  (list env-empty (q-fmap (partial decl:extension name) empty join)))

(define @decl:extension
  ;; should this be a caps-id or a var-id?
  (<$> make-@extension p-caps-id (parens (seq* p-expr (*> comma p-expr)))))

(define @decl:unquote (<$> (partial list env-empty) p-unquo-expr))

(define builtin-@decls
  (hash
    (TID "val")     @decl:val
    (TID "fun")     @decl:fun
    (TID "tag")     @decl:tag
    (TID "rec")     @decl:rec
    (TID "begin")   @decl:begin
    (TID "local")   @decl:local
    (TID "open")    @decl:open
    (TID "extension") @decl:extension
    (TSYM "~")      @decl:unquote))


;; -- exprs --
(provide expr:case-lambda expr:lambda expr:let expr:if expr:case)

(define @expr:parens (<* p-expr rparen))

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
(define-expr lambda (params body)
  [impl (expr:case-lambda (length params)
          (list (list params body))
          (expr:racket `(error "lambda parameter pattern did not match")))]
  [(sexp) `(lambda ,(map pat-sexp params) ,(expr-sexp body))]
  [(compile env) (expr-compile impl env)])

;; for now, lambdas don't get multiple branches, but you can pattern-match.
(define @expr:lambda
  (<$> (q-lift expr:lambda) (parens p-pats) p-expr))

;; (let Decl Expr)
(define-expr let (decl exp)
  [(sexp) `(let ,(decl-sexp decl) ,(expr-sexp exp))]
  [(compile env)
    `(letrec ,(decl-compile decl env)
       ,(expr-compile exp (env-join env (decl-resolveExt decl))))])

(define @expr:let
  (pdo
    `(,e ,d) <- (<* p-decl-group (keyword "in"))
    (<$> (partial (q-lift expr:let) d) (local-env e p-expr))))

;; (if Expr Expr Expr)
(define-expr if (subject then else)
  [(sexp) `(if ,(expr-sexp subject) ,(expr-sexp then) ,(expr-sexp else))]
  [(compile env)
    `(if (truthy? ,(expr-compile subject env))
       ,(expr-compile then env)
       ,(expr-compile else env))])

(define @expr:if
  (<$> (q-lift expr:if) p-expr
    (*> (keyword "then") p-expr)
    (*> (keyword "else") p-expr)))

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
       ,(ir-case env subject-id `(error "No pattern in case matched!")
          branches))])

;; make-@case : Q Expr, [(Q Pat, Q Expr)] -> Q Expr
(define (make-@case subj branches)
  (q-fmap expr:case subj (q-seq (map q-seq branches))))

(define @expr:case
  ;; TODO: shouldn't we be parsing the subject at a certain precedence?
  ;; consider e.g. (case (case x ...) ...)
  (<$> make-@case p-expr
    (many (*> bar (seq* p-pat (*> (keysym "->") p-expr))))))

;; TODO: this should have an extension point!
;; if we did this we could define quasiquote and quote generically, so that
;; anything quasiquotable is automatically quotable!
(define p-quote-form
  (pdo parse-env <- ask
    ext <- (pmap-maybe take-one
             (lambda (t) (hash-lookup t (env-get @quote-forms parse-env)))
             (lambda (t) (format "no quote form bound to ~v" t)))
    (parens ext)))

(define @expr:quasiquote (<$> q-quasi p-quote-form))
(define @expr:quote (<$> q-quo p-quote-form))
(define @expr:unquote p-unquo-expr)

;; TODO: remove this
(define @expr:symbol (<$> (compose q-pure expr:lit) p-any-id))

(define builtin-@exprs
  (hash
    TLPAREN      @expr:parens
    (TSYM "\\")  @expr:lambda
    (TID "let")  @expr:let
    (TID "if")   @expr:if
    (TID "case") @expr:case
    (TSYM "`")   @expr:quasiquote
    (TSYM "`!")  @expr:quote
    (TSYM "~")   @expr:unquote
    (TSYM "'")   @expr:symbol ;; TODO: remove
    ))


;; -- infixes --
(provide expr:seq)

(define @infix-expr:call
  (record
    [precedence 11]
    [(parse func-expr) (<$> (partial (q-lift expr:call) func-expr)
                         (<* (q-listish p-expr) rparen))]))

;; (seq Expr Expr)
(define-expr seq (a b)
  [(sexp) `(begin ,(expr-sexp a) ,(expr-sexp b))]
  [(compile env) `(begin ,(expr-compile a env) ,(expr-compile b env))])

(define @infix-expr:seq
  (record
    [precedence 0]
    [(parse first-expr)
      (<$> (partial (q-lift expr:seq) first-expr) (p-expr-at 0))]))

;; infix operators
;; associativity must be Left or Right.
(define-@infix-expr oper (assoc precedence function)
  [(parse left-arg)
    (<$>
      (lambda (right-arg) (q-fmap (partial expr:call (expr:lit function))
                       (q-seq* left-arg right-arg)))
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

(define builtin-@infix-exprs
  (hash
    TLPAREN    @infix-expr:call
    (TSYM ";") @infix-expr:seq

    ;; TODO: ||, &&, function composition, exponentiation, parser-combinators
    ;; elm-style "|>" operator?

    ;; TODO: make (in)equality operators nonassociative
    ;; TODO: generic inequality operators
    (TSYM "==") (@infix-expr:oper L 5 (compose truthify equal?))
    (TSYM "<")  (@infix-expr:oper L 5 (compose truthify <))
    (TSYM ">")  (@infix-expr:oper L 5 (compose truthify >))
    (TSYM "<=") (@infix-expr:oper L 5 (compose truthify <=))
    (TSYM ">=") (@infix-expr:oper L 5 (compose truthify >=))

    (TSYM "+")  (@infix-expr:oper L 7 +)
    (TSYM "-")  (@infix-expr:oper L 7 -)
    (TSYM "*")  (@infix-expr:oper L 8 *)
    (TSYM "/")  (@infix-expr:oper L 8 /)
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

(define @pat:unquote p-unquo-expr)

(define builtin-@pats
  (hash (TSYM "~") @pat:unquote))


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
(define (@top:nodule resolve-env eng)
  (<$> result:nodule p-caps-id (braces (parse-eval resolve-env eng))))

;; import Name
(define (@top:import resolve-env eng)
  (>>= p-caps-id
    (lambda (name)
      (define result
        (parse-eval-file
          (format "~a.mox" (string-downcase (symbol->string name)))
          (engine-parse-env eng)
          (engine-resolve-env eng)
          eng))
      (return (result:nodule name result)))))

(define (@top:extend resolve-env eng)
  (define (run e)
    (eval (expr-compile (q-run e) resolve-env) (engine-namespace eng)))
  (define (extend e-point e-value)
    (define point (run e-point))
    (define value (run e-value))
    (record
      [resolveExt env-empty]
      [parseExt (env-single point value)]))
  (<$> extend p-expr (*> (keysym "=") p-expr)))

(define (@top:hide resolve-env eng)
  (pdo priv-result <- (parse-eval-one resolve-env eng)
    (local-env (result-parseExt priv-result)
      (parse-eval (env-join resolve-env (result-resolveExt priv-result)) eng))))

(define (@top:private resolve-env eng)
  (pdo priv-result <- (<* (parse-eval resolve-env eng) (keyword "in"))
    (local-env (result-parseExt priv-result)
      (<*
        (parse-eval (env-join resolve-env (result-resolveExt priv-result)) eng)
        (optional (keyword "end"))))))

(define (@top:begin resolve-env eng)
  (<* (parse-eval resolve-env eng) (keyword "end")))

(define builtin-@tops
  (hash
    (TID "module")  @top:nodule
    (TID "import")  @top:import
    (TID "extend")  @top:extend
    (TID "private") @top:private
    (TID "hide")    @top:hide
    (TID "begin")   @top:begin))


;; -- The default/built-in parser extensions --
;; TODO: list, case, dict literals
(provide builtin-parse-env)

(define builtin-parse-env
  (hash
    @decls builtin-@decls
    @exprs builtin-@exprs
    @infix-exprs builtin-@infix-exprs
    @pats builtin-@pats
    @infix-pats builtin-@infix-pats
    @tops builtin-@tops
    @quote-forms builtin-@quote-forms))

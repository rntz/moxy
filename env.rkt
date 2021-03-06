#lang racket

(require (for-syntax (only-in racket/syntax format-id)))

(require "util.rkt")
(require "objects.rkt")
(require "values.rkt")

(provide make-ExtPoint define-ExtPoint)

(define (make-ExtPoint name join empty)
  (ExtPoint name (gensym name) join empty))

(define-syntax-parser (define-ExtPoint name join empty)
  #`(begin
      (define name (make-ExtPoint 'name join empty))
      (define #,(format-id #'name "~a-join" #'name)
        (ExtPoint-join name))
      (define #,(format-id #'name "~a-empty" #'name)
        (ExtPoint-empty name))))


;; Environments are immutable hashtables, mapping extension-points to their
;; monoid values. If an extension-point is absent, it is the same as being
;; mapped to its empty value.

(provide env-empty env-join2 env-join* env-join env-single env-get)

(define env-empty (hash))

(define (env-join2 a b)
  (hash-union a b (lambda (k x y) ((ExtPoint-join k) x y))))

(define (env-join* es) (reduce es env-empty env-join2))
(define (env-join . es) (env-join* es))
(define (env-single ext-point value) (hash ext-point value))
(define (env-get ext-point env)
  (hash-get ext-point env (lambda () (ExtPoint-empty ext-point))))


;; -- Built-in ParseEnv extension points --
;; Convention: extension point names begin with "@"

(provide
  @quote-forms @quote-forms-join @quote-forms-empty
  @exprs @exprs-join @exprs-empty
  @infix-exprs @infix-exprs-join @infix-exprs-empty
  define-@infix-expr @infix-expr @infix-expr-precedence @infix-expr-parse
  @pats @pats-join @pats-empty
  ;; TODO: unify @infix-exprs & @infix-pats somehow?
  @infix-pats @infix-pats-join @infix-pats-empty
  define-@infix-pat @infix-pat @infix-pat-precedence @infix-pat-parse
  @decls @decls-join @decls-empty
  @tops @tops-join @tops-empty

  @nodules @nodules-join @nodules-empty
  define-@nodule @nodule @nodule-name @nodule-resolveExt @nodule-parseExt
  @nodules-nodule @nodule->result
  resolve-nodule-path)

;; Quasiquotation form handlers.
;; Maps tokens to (Parse (Q A)), for some AST type A (eg. expr, decl, pat).
(define-ExtPoint @quote-forms hash-union (hash))

;; Maps tokens to (Parse (Q Expr))
(define-ExtPoint @exprs hash-union (hash))

;; Maps tokens to (@infix-expr)s
(define-ExtPoint @infix-exprs hash-union (hash))
(define-iface @infix-expr
  precedence               ;; Int
  ;; parse : Q Expr -> Parse (Q Expr)
  ;; takes the "left argument" to the infix operator.
  ;; needs to parse the right argument(s) itself.
  ;; so this really allows any non-prefix operator, not just infix
  ;; (postfix or ternary operators, for example)
  (parse left-expr))

;; Maps tokens to (Q Pat)s
(define-ExtPoint @pats hash-union (hash))

(define-ExtPoint @infix-pats hash-union (hash))
(define-iface @infix-pat
  precedence               ;; Int
  ;; parse : Pat -> Parse (Q Pat), see @infix-expr-parser, above
  (parse left-pat))

;; Maps tokens to (Parse (ParseEnv, Q Decl))
(define-ExtPoint @decls hash-union (hash))

;; Maps tokens to (Resolve Env, Engine -> Parse Result)s.
(define-ExtPoint @tops hash-union (hash))

;; Maps symbols to (@nodule)s
(define-ExtPoint @nodules hash-union (hash))
;; eventually, may want to represent @nodules differently, if we have operators
;; on nodules like restricting exported names, etc.
(define-iface @nodule ;; can't use "module", it means something in Racket
  name                                  ;symbol
  resolveExt                            ;ResolveEnv
  parseExt)                             ;ParseEnv

(define-@nodule nodule (name result)
  [resolveExt (result-resolveExt result)]
  [parseExt (result-parseExt result)])

(define (@nodules-nodule name result)
  (hash name (@nodule:nodule name result)))

;; hack that takes advantage of nodules satisfying the result interface
(define (@nodule->result nodule) nodule)

;; returns (Ok nodule)
;; or (Err (parse-env path-prefix path-suffix))
;; assumes path is non-empty
(define (resolve-nodule-path parse-env path)
  (when (null? path) (error 'resolve-nodule-path "can't resolve empty path"))
  (let loop ([parse-env parse-env]
             [prev '()]
             [next path])
    (match-define (cons name rest) next)
    (match (hash-lookup name (env-get @nodules parse-env))
      [(Just nodule) (if (null? rest)
                       (Ok nodule)
                       (loop (@nodule-parseExt nodule) (cons name prev) rest))]
      [(None) (Err `(,parse-env ,(reverse prev) ,next))])))


;; -- Built-in ResolveEnv extension points --

;; FIXME: should ResolveEnv really be represented as an env? or is it too
;; special-purpose? What legitimate extensions to ResolveEnv are possible?

(provide
  @vars @vars-join @vars-empty
  define-@var @var @var-style @var-id
  @var-tag-id @var-tag-params @var-tag-arity)

;; Maps symbols to (@var)s.
(define-ExtPoint @vars hash-union (hash))

;; Info about a name. Ubiquitous fields:
;; - style: one of '(var ctor)
;; - id: the IR identifier for the value of this variable.
;;
;; Fields for ctors only:
;; - tag-id: The IR id for the tag for this ctor.
;; - tag-params: (Maybe [Symbol]). The parameters for the ctor, if any.
(define-iface @var style id)
(define-accessors @var tag-id tag-params) ;not-always-present fields

(define (@var-tag-arity v [or-else #f])
  (maybe (@var-tag-params v) 0 length))


;; Builtin parts of speech.
;;
;; "Parts of speech" are interfaces for various parts of the language AST; e.g.
;; expressions, declarations, patterns.
;;
;; Parts of speech are defined by the interface they present so that people can
;; add new forms with new behavior. E.g. an Expr is anything that has a 'compile
;; "method" that takes a ResolveEnv and produces an IR expression.

(provide
  define-var var var-sexp var-resolve
  define-expr expr expr-compile expr-sexp
  define-decl decl decl-sexp decl-resolveExt decl-compile
  define-pat pat pat-sexp pat-resolveExt pat-idents pat-compile
  define-result result result-resolveExt result-parseExt)

;; a var is something that knows how to resolve itself to a @var
(define-iface var
  (sexp)
  (resolve env or-else))                ; ResolveEnv, thunk -> @var

(define-iface expr
  (sexp)
  (compile resolve-env))                ; ResolveEnv -> IR

(define-iface decl
  (sexp)
  resolveExt                            ; ResolveEnv

  ;; The rest of our interface would ideally be:
  ;;
  ;;   idents: [Id]
  ;;   compile: ResolveEnv -> IR
  ;;
  ;; where `idents' is a list of the identifiers bound, and (compile env)
  ;; returns code which evaluates to a "tuple" of the values each identifier
  ;; should have. Practically, however, this requires either (a) allocating a
  ;; tuple or (b) using multiple-return-values. I'd like to avoid (a) because it
  ;; sucks and (b) because it's very racket-specific (if we designed our own IR
  ;; it would be difficult to duplicate - MRV is hard to implement efficiently).
  ;;
  ;; Also it makes it slightly harder to define mutually recursive functions.
  ;;
  ;; So instead we do this:
  ;;
  ;;   compile: ResolveEnv -> [(Id, IR)]
  ;;
  ;; Returns a list of identifier-IR pairs, (id code). `code' is IR that
  ;; evaluates to what `id' should be bound to. Each `id' is in the scope of
  ;; every `code', but the `id's are defined in the order given (a la letrec).
  ;;
  ;; And no, there's no reason in particular that we don't do it the following
  ;; way instead:
  ;;
  ;;   idents: [Id]
  ;;   compile: ResolveEnv -> [IR]
  ;;
  ;; It would just be annoying to change all the code at this point.
  (compile resolve-env))

(define-iface pat
  (sexp)
  ;; can patterns really modify our resolve environment in arbitrary ways?
  ;;
  ;; relatedly: how do I know what identifiers a pattern binds (say I need to
  ;; use it in a val decl, say)?
  resolveExt                            ; ResolveEnv

  ;; The rest of our interface would ideally be something like:
  ;;
  ;;   idents: [Id]
  ;;   compile: ResolveEnv, IR -> IR
  ;;
  ;; Where `idents' is a list of identifiers bound, and (compile env subject)
  ;; returns code that matches against `subject' and evaluates to either (Just
  ;; t) where `t' is a tuple of the values the `idents' should be bound to, or
  ;; None, indicating pattern-match failure.
  ;;
  ;; But this always requires allocating, which sucks. So instead we do this:
  ;;
  ;;   idents: [Id]
  ;;   compile: ResolveEnv, IR, IR, IR -> IR
  ;;
  ;; Where (compile env subject on-success on-failure) returns code that matches
  ;; against `subject', binds the identifiers in `idents' and runs `on-success';
  ;; on pattern-match failure, it runs `on-failure' (it may have bound none,
  ;; some, or all of `idents').
  ;;
  ;; `subject', `on-success' and `on-failure' may occur many times in the
  ;; returned code. So they should be small (e.g. literals, identifiers or
  ;; zero-argument calls to identifiers), and `subject' must be
  ;; side-effect-less.
  ;;
  ;; EDIT: for now I'm letting `on-success' be large. This shouldn't be a
  ;; problem except with patterns that can succeed in multiple ways (e.g.
  ;; or-patterns). So once I implement those I'll reconsider this.
  idents                                ;[Id], represented as a racket list
  (compile resolve-env subject on-success on-failure))

;; The "result" of parsing a top-level declaration. Not exactly a part of
;; speech, but acts like one.
(define-iface result
  resolveExt
  parseExt)

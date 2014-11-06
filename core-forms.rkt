#lang racket

(require "util.rkt")
(require "values.rkt")
(require "env.rkt")

;; The part-of-speech forms that the parser uses natively, without any
;; extensions (even built-in ones).

(provide
  @var:var @var:ctor @vars-var @vars-ctor
  var:local var:unbound var:qualified
  expr:var expr:lit expr:racket expr:call
  pat:lit pat:var pat:vector pat:tagged)

(define (literal? x)
  (or (string? x) (number? x) (procedure? x)))


;; - @vars -
(define-@var var (name id) [style 'var])
(define-@var ctor (name id tag-id tag-params) [style 'ctor])

(define (@vars-var name id) (hash name (@var:var name id)))
(define/contract (@vars-ctor name id tag-id tag-params)
  (-> symbol? symbol? symbol? (Maybe/c (listof symbol?)) hash?)
  (hash name (@var:ctor name id tag-id tag-params)))


;; - vars -
(define-var local (name)
  [(sexp) name]
  [(resolve env or-else)
    (hash-get name (env-get @vars env) or-else)])

(define-var unbound (name)
  [(sexp) `(unbound ,name)]
  [(resolve env or-else) (or-else)])

(define-var qualified (nodule var)
  [(sexp)
    ;; TODO: this is an ugly hack.
    (let ((x (var-sexp var)))
      (if (symbol? x)
        (string->symbol (format "~a.~a" (@nodule-name nodule) x))
        (cons (@nodule-name nodule) x)))]
  [(resolve env or-else)
    (var-resolve var (@nodule-resolveExt nodule) or-else)])


;; - exprs -
(define-expr var (var)                 ;var -> expr
  [(sexp) (var-sexp var)]
  [(compile env)
    (@var-id (var-resolve var env
               (lambda () (error 'expr:var "unbound variable ~a"
                       (show (var-sexp var))))))])

(define-expr lit (value)
  [(sexp) (if (literal? value) value (list 'quote value))]
  [(compile env) (list 'quote value)])

(define-expr racket (code)
  [(sexp) `(racket ,code)]
  [(compile env) code])

;; (call Expr [Expr])
(define-expr call (func args)
  [(sexp) (map expr-sexp (cons func args))]
  [(compile env) (map (lambda (x) (expr-compile x env)) (cons func args))])


;; - pats -
(define-pat var (name)
  [(sexp) name]
  [id (mkid name)]
  [resolveExt (env-single @vars (@vars-var name id))]
  [idents (list id)]
  [(compile env subject on-success on-failure)
    `(let ([,id ,subject]) ,on-success)])

(define-pat lit (value)
  [(sexp) (if (literal? value) value (list 'quote value))]
  [resolveExt env-empty]
  [idents '()]
  [(compile env subject on-success on-failure)
    `(if (equal? ,subject ',value)
       ,on-success
       ,on-failure)])

;; TODO: pat:vector, pat:tagged shouldn't have to be core forms :(
(define-pat vector (elems)
  [(sexp) `(vector ,@(map pat-sexp elems))]
  [resolveExt (env-join* (map pat-resolveExt elems))]
  [idents (append* (map pat-idents elems))]
  [(compile env subject on-success on-failure)
    (let ([elems (list->vector elems)])
      `(if (and (vector? ,subject)
                (= (vector-length ,subject) ',(vector-length elems)))
         ,(let loop ([i 0] [env env])
            (if (>= i (vector-length elems)) on-success
              (let ([tmp (mktemp 'vector-elem)]
                     [elem (vector-ref elems i)])
                `(let ([,tmp (vector-ref ,subject ',i)])
                   ;; NOTE: this calls pat-compile with a potentially large
                   ;; on-success code. but on-success should always be small.
                   ;; how do we fix this? do we even bother?
                   ,(pat-compile elem env tmp
                      (loop (+ i 1) (env-join env (pat-resolveExt elem)))
                      on-failure)))))
         ;; not a vector or wrong length
         ,on-failure))])

;; (tagged var:Var params:[Pat])
(define-pat tagged (var params)
  [(sexp) `(,(var-sexp var) ,@(map pat-sexp params))]
  [arity (length params)]
  [params-pat (pat:vector params)]
  [resolveExt (pat-resolveExt params-pat)]
  [idents (pat-idents params-pat)]
  [(compile env subject on-success on-failure)
    (let* ([info (var-resolve var env
                   (lambda () (error 'pat:tagged "unbound tag ~a"
                           (show (var-sexp var)))))]
           [var-id (@var-id info)]
           [tag-id (@var-tag-id info
                     (lambda () (error 'pat:tagged "~a is not a tag"
                             (show (var-sexp var)))))]
           [tag-arity (@var-tag-arity info)]
           [tmp (mktemp 'tagged-value)])
      (unless (= arity tag-arity)
        (error 'pat:tagged
          "Constructor pattern has ~a arguments (had ~a, expected ~a)"
          (if (< arity tag-arity) "too few" "too many")
          arity tag-arity))
      `(if (tagged-isa? ,tag-id ,subject)
         ;; match each pat in params against (tagged-value subject)
         (let ((,tmp (tagged-value ,subject)))
           ,(pat-compile params-pat env tmp on-success on-failure))
         ,on-failure))])

#lang racket

(require "util.rkt")
(require "values.rkt")
(require "env.rkt")

;; The part-of-speech forms that the parser uses natively, without any
;; extensions (even built-in ones).

(provide expr:var expr:lit pat:lit pat:var pat:vector pat:ann)

;; - exprs -
(define-form expr:var (name)
  [(sexp) name]
  [(compile env)
    ;; TODO: better error handling
    (hash-get 'id
      (hash-get name (env-get @vars env)
        (lambda () (error 'expr:var "unbound variable ~v" name))))])

(define-form expr:lit (value)
  [(sexp) (if (or (string? value) (number? value)) value (list 'quote value))]
  [(compile env) (list 'quote value)])

;; - pats -
(define-form pat:var (name)
  [id (gensym name)]
  [resolveExt (env-single @vars (@vars-var name id))]
  [idents (list id)]
  [(compile env subject on-success on-failure)
    `(let ([,id ,subject]) ,on-success)])

(define-form pat:lit (value)
  [(sexp) (if (or (string? value) (number? value)) value (list 'quote value))]
  [resolveExt env-empty]
  [idents '()]
  [(compile env subject on-success on-failure)
    `(if (equal? ,subject ',value)
       ,on-success
       ,on-failure)])

;; TODO: pat:vector, pat:ann shouldn't have to be core forms :(
(define-form pat:vector (elems)
  [resolveExt (env-join* (map pat-resolveExt elems))]
  [idents (append* (map pat-idents elems))]
  [(compile env subject on-success on-failure)
    (let ([elems (list->vector elems)])
      `(if (and (vector? ,subject)
                (= (vector-length ,subject) ',(vector-length elems)))
         ,(let loop ([i 0] [env env])
            (if (>= i (vector-length elems)) on-success
              (let ([tmp (gensym 'tmp:vector-elem)]
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

;; (ann name:Symbol params:[Pat])
(define-form pat:ann (name params)
  [arity (length params)]
  [params-pat (pat:vector params)]
  [resolveExt (pat-resolveExt params-pat)]
  [idents (pat-idents params-pat)]
  [(compile env subject on-success on-failure)
    (let* ([info (hash-get name (env-get @vars env)
                   (lambda () (error 'pat:ann "unbound ctor ~v" name)))]
           [ctor-id (@var-id info)]
           [tag-id (@var-tag-id info
                     (lambda () (error 'pat:ann "~v is not a ctor" name)))]
           [tag-arity (@var-tag-arity info)]
           [tmp (gensym 'tmp)])
      (unless (= arity tag-arity)
        (error 'pat:ann
          "Constructor pattern has ~a arguments (had ~a, expected ~a)"
          (if (< arity tag-arity) "too few" "too many")
          arity tag-arity))
      `(if (ann-isa? ,tag-id ,subject)
         ;; match each pat in params against (ann-args subject)
         (let ((,tmp (ann-args ,subject)))
           ,(pat-compile params-pat env tmp on-success on-failure))
         ,on-failure))])

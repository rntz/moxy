#lang racket

(require "util.rkt")
(require "values.rkt")
(require "env.rkt")

;; The part-of-speech forms that the parser uses natively, without any
;; extensions (even built-in ones).

(provide expr:var expr:lit)

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

#lang racket

(require racket/stream)

(require "debug.rkt")
(require "util.rkt")
(require "values.rkt")
(require "objects.rkt")
(require "env.rkt")
(require "lex.rkt")
(require "pcomb.rkt")
(require "parse.rkt")
(require "parse-builtins.rkt")
(require "runtime.rkt")
(require "repl.rkt")

(define (parse-expr what) (expr-sexp (parse-expr-no-really what)))
(define (parse-pat what)  (pat-sexp  (parse-pat-no-really what)))
(define (parse-decl what) (decl-sexp (parse-decl-no-really what)))

(define (parse-expr-no-really what) (parse p-expr builtin-parse-env what))
(define (parse-pat-no-really what)  (parse p-pat  builtin-parse-env what))
(define (parse-decl-no-really what) (parse p-decl builtin-parse-env what))

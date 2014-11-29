#lang racket

(require racket/stream)

(require "debug.rkt")
(require "env.rkt")
(require "lex.rkt")
(require "objects.rkt")
(require "parse-builtins.rkt")
(require "parse.rkt")
(require "pcomb.rkt")
(require (only-in "quasi.rkt" run))
(require "repl.rkt")
(require "runtime.rkt")
(require "tags.rkt")
(require "util.rkt")
(require "values.rkt")

(define (tokenify what)
  (cond
    [(stream? what) what]
    [(port? what) (tokenize what)]
    [(string? what) (call-with-input-string what tokenize)]
    [#t (error 'parse "don't know how to parse: ~v" what)]))

(define (p parser env what [whole #t])
  (parse (if whole (<* parser peof) parser) env (tokenify what)))

(define (parse-expr what) (expr-sexp (parse-expr-no-really what)))
(define (parse-pat what)  (pat-sexp  (parse-pat-no-really what)))
(define (parse-decl what) (decl-sexp (parse-decl-no-really what)))

(define (parse-expr-no-really what) (run (p p-expr builtin-parse-env what)))
(define (parse-pat-no-really what)  (run (p p-pat  builtin-parse-env what)))
(define (parse-decl-no-really what) (run (p p-decl builtin-parse-env what)))

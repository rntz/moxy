#!/usr/bin/env racket
#lang racket

(require racket/stream)

(require "util.rkt")
(require "debug.rkt")
(require "values.rkt")
(require "env.rkt")
(require "lex.rkt")
(require "pcomb.rkt")
(require "parse.rkt")
(require "parse-builtins.rkt")
(require "runtime.rkt")

(define-tag FoundSemi rev-toks after)
(define-tag NeedMore accum paren-level)
(define-tag UnbalancedParens)

(define (find-semi toks accum paren-level)
  (match toks
    ['() (NeedMore accum paren-level)]
    [(cons tok toks)
      (match tok
        [(? (const (= 0 paren-level)) (TSYM ";"))
          (FoundSemi accum toks)]
        [(or (TLPAREN) (TLBRACK) (TLBRACE))
          (find-semi toks (cons tok accum) (+ paren-level 1))]
        [(or (TRPAREN) (TRBRACK) (TRBRACE))
          (if (= 0 paren-level) UnbalancedParens
            (find-semi toks (cons tok accum) (- paren-level 1)))]
        [_ (find-semi toks (cons tok accum) paren-level)])]))

(define-tag Result result)
(define-tag Expr expr)

(define (repl-display-@vars vars ns)
  (for ([(name info) vars])
    (match (hash-get 'style info (lambda () #f))
      ['var (void)]
      ['ctor
        (printf "tag ~a" name)
        (match (@var-tag-params info)
          [(Just params)
            (printf "(~a)\n" (string-join (map symbol->string params) ", "))]
          [_ (printf "\n")])]
      [_ (printf "Oh my, what have you done to ~a?!\n" name)])
    (if (hash-has? 'id info)
      (printf "val ~a = ~a\n" name (show (eval (@var-id info) ns)))
      (printf "I don't even know what value ~a has!\n" name))))

(define (repl [hack #t])
  (when hack
    (read-line)) ;; crude hack to make this work inside the racket repl
  (define-values (resolve-env ns) (make-runtime))
  (let eval-next ([parse-env builtin-parse-env]
                  [resolve-env resolve-env]
                  [toks '()])
    (define (print-error loc msg)
      ;; TODO?: make locations work
      (eprintf "error: ~a\n" msg)
      (result:empty))
    (define (handle loc res)
      ;; print what got bound
      (match res
        [(Expr e)
          (debugf-pretty " ** AST:" (expr-sexp e))
          (define code (expr-compile e resolve-env))
          (debugf-pretty " ** IR:" code)
          (define value (eval code ns))
          (unless (void? value)
            (printf "~a\n" (show value)))
          (result:empty)]
        [(Result result)
          (repl-display-@vars (env-get @vars (result-resolveExt result)) ns)
          result]))
    (define (parse-eval-toks toks)
      ((choice
         (<$> Result (<* (parse-eval resolve-env ns) peof))
         (<$> Expr (<* p-expr peof)))
        parse-env
        (stream-stream (in-list toks))
        print-error
        print-error
        handle))
    (let get-expr ([toks toks]
                   [accum '()]
                   [paren-level 0])
      (match (find-semi toks accum paren-level)
        [(FoundSemi rev-toks toks)
          ;; We found the end of the toplevel decl/expression!
          (let ([result (parse-eval-toks (reverse rev-toks))])
            (eval-next (env-join parse-env (result-parseExt result))
                       (env-join resolve-env (result-resolveExt result))
                       toks))]
        [(UnbalancedParens)
          ;; Send an error message & start over
          (eprintf "Err, your parentheses are unbalanced.\n")
          (get-expr '() '() 0)]
        [(NeedMore accum paren-level)
          ;; Grab a line and keep looking for the semi
          ;;(printf (if (null? accum) "MLOID: " "   ... "))
          (printf (if (null? accum) "- " "= "))
          (define line (read-line))
          (if (eof-object? line) (eprintf "Bye!\n")
            (get-expr (stream->list (call-with-input-string line tokenize))
                      accum
                      paren-level))]))))

(module+ main
  ;; enable line counting because it makes pretty printing better
  (port-count-lines! (current-error-port))
  (port-count-lines! (current-output-port))
  (repl #f))

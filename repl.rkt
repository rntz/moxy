#lang racket

(require racket/stream)

(require "util.rkt")
(require "values.rkt")
(require "env.rkt")
(require "pcomb.rkt")
(require "parse.rkt")
(require "lex.rkt")

(define init-parse-env builtin-parse-env)
(define init-resolve-env env-empty)

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
        [(TLPAREN) (find-semi toks (cons tok accum) (+ paren-level 1))]
        [(TRPAREN) (if (= 0 paren-level) UnbalancedParens
                     (find-semi toks (cons tok accum) (- paren-level 1)))]
        [_ (find-semi toks (cons tok accum) paren-level)])]))

(define (repl)
  (read-line) ;; crude hack to make this work inside the racket repl
  (let eval-next ([parse-env init-parse-env]
                  [resolve-env init-resolve-env]
                  [toks '()])
    (define (print-error loc msg)
      ;; TODO?: make locations work
      (eprintf "error: ~a\n" msg)
      (result:empty))
    (define (handle-result loc result)
      (printf "result: ~v\n" result)  ;FIXME
      result)
    (define (parse-eval-toks toks)
      ((<* (parse-eval resolve-env) peof)
        parse-env
        (stream-stream (in-list toks))
        print-error
        print-error
        handle-result))
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

#!/usr/bin/env racket
#lang racket

(require racket/stream)

(require "debug.rkt")
(require "util.rkt")
(require "values.rkt")
(require "env.rkt")
(require "lex.rkt")
(require "pcomb.rkt")
(require "parse.rkt")
(require "parse-builtins.rkt")
(require "runtime.rkt")

(provide repl)

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

(define (show-result ns result [indent 0])
  (show-@nodules ns (env-get @nodules (result-parseExt result)) indent)
  (show-@vars ns (env-get @vars (result-resolveExt result)) indent))

(define (show-@nodules ns nodules indent)
  (define prefix (string->immutable-string (make-string (* 2 indent) #\space)))
  (define (indent-printf fmt . args)
    (apply printf (string-append prefix fmt) args))
  (for ([(name nodule) nodules])
    ;; TODO: empty modules should display as "module NAME {}"
    (indent-printf "module ~a {\n" name)
    (show-result ns (@nodule->result nodule) (+ 1 indent))
    (indent-printf "}\n")))

(define (show-@vars ns vars indent)
  (define prefix (string->immutable-string (make-string (* 2 indent) #\space)))
  (define (indent-printf fmt . args)
    (apply printf (string-append prefix fmt) args))
  (for ([(name info) vars])
    (match (hash-get 'style info (lambda () #f))
      ['var (void)]
      ['ctor
        (indent-printf "tag ~a" name)
        (match (@var-tag-params info)
          [(Just params)
            (printf "(~a)\n" (string-join (map symbol->string params) ", "))]
          [_ (printf "\n")])]
      [_ (indent-printf "Oh my, what have you done to ~a?!\n" name)])
    (if (hash-has? 'id info)
      (indent-printf "val ~a = ~a\n" name (show (eval (@var-id info) ns)))
      (indent-printf "I don't even know what value ~a has!\n" name))))

;; TODO: refactor this
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
          (debugf-pretty " * AST:" (expr-sexp e))
          (define code (expr-compile e resolve-env))
          (debugf-pretty " * IR:" code)
          (define value (eval code ns))
          (unless (void? value)
            (printf "~a\n" (show value)))
          (result:empty)]
        [(Result result)
          (show-result ns result)
          result]))
    (define (parse-eval-toks toks)
      (with-handlers ([(const #t)
                        (lambda (exn)
                          ;; TODO: only display backtrace for internal errors -
                          ;; but how?
                          ((error-display-handler) (exn-message exn) exn)
                          (result:empty))])
        ((choice
           (<$> Result (<* (parse-eval resolve-env ns) peof))
           (<$> Expr (<* p-expr peof)))
          parse-env
          (stream-stream (in-list toks))
          print-error
          print-error
          handle)))
    (let get-expr ([toks toks]
                   [accum '()]
                   [paren-level 0])
      (match (find-semi toks accum paren-level)
        [(FoundSemi rev-toks toks)
          ;; We found the end of the toplevel decl/expression!
          (debugf " * TOKS: ~a" (show (reverse rev-toks)))
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
          (define line
            ;; TODO: check behavior inside racket repl.
            ;; FIXME: should only catch ^C (SIGINT) breaks, not hang-ups or
            ;; terminates.
            (with-handlers ([exn:break? (const None)])
              (Just (read-line))))
          (match line
            [(Just x)
              (if (eof-object? x) (eprintf "Bye!\n")
                (get-expr
                  (stream->list (call-with-input-string x tokenize))
                  accum
                  paren-level))]
            [(None)
              (eprintf "Interrupted!\n")
              (get-expr '() '() 0)])]))))

(module+ main
  ;; enable line counting because it makes pretty printing better
  (port-count-lines! (current-error-port))
  (port-count-lines! (current-output-port))
  (undebug!)
  (repl #f))

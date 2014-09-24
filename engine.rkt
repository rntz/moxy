#lang racket

(provide (struct-out engine))

;; we need the namespace to pass to 'eval.
;;
;; we need {parse,resolve}-env when we load a new file or start a new repl, so
;; that it sees only the prelude environment.
(define-struct engine (namespace parse-env resolve-env)
  #:prefab)

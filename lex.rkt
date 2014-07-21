#lang racket

;; uses package: parser-tools-lib
(require racket/stream)
(require parser-tools/lex)
(require (prefix-in : parser-tools/lex-sre))

(require "values.rkt")

;; Representing tokens
(provide
  tag:TEOF TEOF TEOF?
  tag:TLPAREN TLPAREN TLPAREN? tag:TRPAREN TRPAREN TRPAREN?
  tag:TLBRACK TLBRACK TLBRACK? tag:TRBRACK TRBRACK TRBRACK?
  tag:TLBRACE TLBRACE TLBRACE? tag:TRBRACE TRBRACE TRBRACE?
  tag:TID TID TID? TID-value
  tag:TSYM TSYM TSYM? TSYM-value
  tag:TNUM TNUM TNUM? TNUM-value
  tag:TSTR TSTR TSTR? TSTR-value)

(define-tag TEOF)
(define-tag TLPAREN) (define-tag TRPAREN)
(define-tag TLBRACK) (define-tag TRBRACK)
(define-tag TLBRACE) (define-tag TRBRACE)

(define-tag TID value)
(define-tag TSYM value)
(define-tag TNUM value)
(define-tag TSTR value)


;; The actual lexing
(provide tokenize tokenize-with-position dump)

(define (tokenize port)
  (stream-map position-token-token (tokenize-with-position port)))

(define (tokenize-with-position port)
  (let ([next (yak-lex port)])
    (if (TEOF? next) empty-stream
      (stream-cons next (tokenize-with-position port)))))

(define (dump port)
  (stream->list (tokenize port)))

(define yak-lex
  (lexer-src-pos
    ;; Whitespace & comments are ignored, except newlines
    [(:+ whitespace) (return-without-pos (yak-lex input-port))]
    [comment (return-without-pos (yak-lex input-port))]
    ;; Simple cases
    [(eof) (return-without-pos TEOF)]
    ["(" TLPAREN]   [")" TRPAREN]
    ["[" TLBRACK] ["]" TRBRACK]
    ["{" TLBRACE]   ["}" TRBRACE]
    ;; Complex cases
    [ident (TID lexeme)]
    [symbol (TSYM lexeme)]
    [number (TNUM (string->number lexeme))]
    ["\"" (TSTR (str-lex input-port))]))

(define-lex-abbrevs
  [eol "\n"]
  [comment (:seq "#" (:* (:~ eol)))]
  ;; [inline-space (:& whitespace (:~ eol))]
  [nat (:+ numeric)]
  [ident-init (:or alphabetic (char-set "_"))]
  [ident-mid  (:or ident-init numeric)]
  [ident (:seq ident-init (:* ident-mid))]
  [symbol (:+ (char-set "`~!@$%^&*-_=+\\:<>/?|,.;"))]
  ;; Might want to loosen number definition.
  ;; Currently rejects: ".0" "1." "-.0" etc.
  ;; Note that "-12.-3" lexes as: (-12 .- 3)
  [number (:seq (:or "" (char-set "+-"))
                nat
                (:or "" (:seq "." nat)))])

(define (str-lex port)
  (let loop ([strs '()])
    (let ([next (str-char-lex port)])
      (if next (loop (cons next strs))
        (apply string-append (reverse strs))))))

(define str-char-lex
  (lexer
    [(:* (:~ (char-set "\\\""))) lexeme]
    ;; TODO: escape sequences
    [(:seq "\\" any-char) (string (string-ref lexeme 1))]
    ["\"" #f]                           ;end of string
    ;; TODO: better errors
    [(eof) (raise 'exn:read)]
    [any-char (raise 'exn:read)]))

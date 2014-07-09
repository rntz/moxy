#lang racket

(provide tokenize dump)

;; uses package: parser-tools-lib
(require racket/stream)
(require parser-tools/lex)
(require (prefix-in : parser-tools/lex-sre))

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

(define (tokenize port)
  (let ([next (yak-lex port)])
    (if (eq? 'eof next) empty-stream
      (stream-cons next (tokenize port)))))

(define (dump port)
  (map position-token-token (stream->list (tokenize port))))

(define yak-lex
  (lexer-src-pos
    ;; Whitespace & comments are ignored, except newlines
    [(:+ whitespace) (return-without-pos (yak-lex input-port))]
    [comment (return-without-pos (yak-lex input-port))]
    ;; Simple cases
    [(eof) (return-without-pos 'eof)]
    ["(" 'lparen]   [")" 'rparen]
    ["[" 'lbracket] ["]" 'rbracket]
    ["{" 'lbrace]   ["}" 'rbrace]
    ;; Complex cases
    [ident `(ident ,lexeme)]
    [symbol `(symbol ,lexeme)]
    [number `(number ,(string->number lexeme))]
    ["\"" `(string ,(str-lex input-port))]))

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

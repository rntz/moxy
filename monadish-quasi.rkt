#lang racket

(require (for-syntax (only-in racket/function identity)))
(require racket/stxparam (for-syntax syntax/parse))

(require "util.rkt")
(require "monadish.rkt")


;; TODO: get these to stack! so that unquote & quasiquote can be interesting!
;; Okay, now let's try actually using these.
(define-syntax-parameter current-monadish
  (make-rename-transformer #'id-monad))

(define-syntax-rule (m-pure x) ((Idiom-pure current-monadish) x))
(define-syntax-rule (m-map f a ...) ((Idiom-map current-monadish) f a ...))
(define-syntax-rule (m-ap f a ...) ((Idiom-ap current-monadish) f a ...))
(define-syntax-rule (m-join e) ((Monad-join current-monadish) e))
(define-syntax-rule (m-call f a ...) (((Monad-bind current-monadish) f) a ...))
(define-syntax-parser m-last
  [(_ e) #'e]
  [(_ es ... e) #'((Idiom-last current-monadish) es ... e)])
(define-syntax-rule (m-let ((name expr) ...) body ...)
  (m-call (lambda (name ...) body ...) expr ...))

(define-syntax-rule (m<- p e body ...) (m-call (match-lambda [p body ...]) e))

(define-syntax-rule (m-in monadish e)
  (let ([the-monadish monadish])
    (syntax-parameterize ([current-monadish
                            (make-rename-transformer #'the-monadish)])
      e)))

(define-syntax-parser m
  [(_ #:in monadish body ...+) #'(m-in monadish (m-body body ...))]
  [(_ body ...+) #'(m-body body ...)])

(begin-for-syntax
  (define-syntax-class define-head
    (pattern name:id #:attr transform identity)
    (pattern (head:define-head param:id ...)
      #:attr name #'head.name
      #:attr transform
      (lambda (x) #`(lambda (param ...) #,((attribute head.transform) x)))))

  (define-syntax-class definition
    #:datum-literals (define)
    (pattern (define head:define-head body ...)
      #:attr name #'head.name
      #:attr value ((attribute head.transform) #'(begin body ...)))))

(define-syntax-parser m-body
  ;; a body may begin with a list of definitions
  #:datum-literals (<-)
  [(_ ds:definition ...+ (~and body (~not _:definition)) ...)
    #'(m-expr (letrec ([ds.name ds.value] ...) body ...))]
  [(_ (p <- e) body ...) #'(m<- p (m-expr e) (m-body body ...))]
  [(_ ks ... k) #'(m-last (m-stmt ks) ... (m-expr k))])

(define-syntax-rule (m-stmt e) (m-expr e))

(define-syntax-parser (m-expr e)
  (syntax-parse #'e
    #:datum-literals (begin unquote unquote-splicing let let* lambda <-)
    [(unquote e) #'e]
    [(let ([name expr] ...) body ...)
      #'(m-let ([name (m-expr expr)] ...) (m-body body) ...)]
    [(let* () body ...) #'(m-body body ...)]
    [(let* ([n e] [name expr] ...) body ...)
      #'(m-expr (let ([n e]) (let* ([name expr] ...) body ...)))]
    [(begin body ...) #'(m-body body ...)]
    [((~datum <-) p e body ...)
      ;; TODO: call fail (if available) on pattern-match failure
      ;; #'(m-call (match-lambda [p (m-body body ...)]) (m-expr e))
      #'(m<- p (m-expr e) (m-body body ...))]
    [((~datum @) e) #'(m-join (m-expr e))]
    ;; TODO: check that func isn't bound in transformer environment
    [(func:id arg ...) #'(m-map func (m-expr arg) ...)]
    [(func arg ...) #'(m-ap (m-expr func) (m-expr arg) ...)]
    [(~or x:id x:str x:boolean x:str x:char x:number) #'(m-pure x)]))


;; (define-syntax-rule (m-syntaxify long-name-so-it-wont-get-shadowed ...)
;;   (let-syntax
;;     (...
;;       ([#%app (syntax-parser
;;                 ;; [(_ f:id a ...) #'(m-map f a ...) ]
;;                 [(_ f a ...) #'(m-ap f a ...)])]
;;         [quote (make-rename-transformer #'m-pure)]
;;         ;; TODO: begin should maybe try to handle defines?
;;         [begin (make-rename-transformer #'m-last)]
;;         ;; TODO: unquote
;;         [<- (syntax-rules ()
;;               [(_ p e body ...) (m-call (match-lambda [p body ...]) e)])]))
;;     long-name-so-it-wont-get-shadowed ...))

;; (define-syntax (m-syntax stx)
;;   (define/syntax-parse (_ long-name-so-it-wont-shadow ...) stx)
;;   (define/syntax-parse app (datum->syntax stx '#%app))
;;   (define/syntax-parse quo (datum->syntax stx 'quote))
;;   (define/syntax-parse beg (datum->syntax stx 'begin))
;;   syntax-parse
;;   #'(let-syntax
;;       (...
;;         ([#%app (syntax-parser
;;                   ;; [(_ f:id a ...) #'(m-map f a ...) ]
;;                   [(_ f a ...) #'(m-ap f a ...)])]
;;           [quo (make-rename-transformer #'m-pure)]
;;           ;; TODO: begin should maybe try to handle defines?
;;           [beg (make-rename-transformer #'m-last)]
;;           ;; TODO: unquote
;;           [<- (syntax-rules ()
;;                 [(_ p e body ...) (m-call (match-lambda [p body ...]) e)])]))
;;       long-name-so-it-wont-get-shadowed ...))

;; (begin-for-syntax
;;   (define m-let
;;     (syntax-parser
;;       [(_ (b:binding ...) body ...)
;;         #'((m-body body ...))]
;;       )))

;;; Miscellaneous utilities
(define (const x) (lambda args x))
(define (println x) (print x) (display "\n"))
(define (repr x)
  (with-output-to-string (lambda () (write x))))

;;; General syntax
(define-syntax le-accum##               ;the ## is for ugliness
  (syntax-rules ()
    [(_ acc e) (letrec acc e)]
    [(_ (acc ...) id ex rest ... e)
      (le-accum## (acc ... (id ex)) rest ... e)]))

(define-syntax le
  (syntax-rules ()
    [(le bindings ... exp)
      (le-accum## () bindings ... exp)]))

(define-syntax-rule (le1 x e body ...) (let ((x e)) body ...))

(define-syntax-rule (matches? exp pat)
  (match exp [pat #t] [_ #f]))

(define-syntax-rule (lambda-rec name rest ...)
  (letrec ((name (lambda rest ...))) name))

;;; Syntax for interfaces
(define-syntax define-interface
  (syntax-rules ()
    [(define-interface iface-name parents method ...)
      (begin
        (define iface-name (interface parents method ...))
        (define-methods iface-name method ...))]))

(define-syntax define-methods
  (syntax-rules ()
    [(define-methods iface-name) (begin)]
    [(define-methods iface-name method methods ...)
      (begin
        (define-method iface-name method)
        (define-methods iface-name methods ...))]))

(define-syntax define-method
  (syntax-rules ()
    [(define-method iface-name (method contract))
      (define-method iface-name method)]
    [(define-method iface-name method)
      (define method
        (let ([g (generic iface-name method)])
          (lambda (object . args)
            (send-generic object g . args))))]))



;;; First we define what our stream interface looks like.
(define-interface stream<%> ()
  read-char   ;; stream -> (or char? eof-object?)
  read-string ;; stream nat -> string?
  get-pos     ;; stream -> pos
  restore     ;; stream pos -> ()
  eof?        ;; stream -> bool
  )

(define string-stream%
  (class* object% (stream<%>)
    (init string)

    (define contents string)
    (define current-pos 0)
    (super-new)

    (define/public (read-char)
      (if (eof?) eof
        (begin0
          (string-ref contents current-pos)
          (set! current-pos (+ 1 current-pos)))))

    (define/public (read-string amt)
      (let ([start current-pos]
            [end (min (+ current-pos amt) (string-length contents))])
        (set! current-pos end)
        (substring contents start end)))

    (define/public (get-pos) current-pos)

    (define/public (restore pos)
      (and (< pos 0) (> pos (string-length contents))
        (error "invalid position"))
      (set! current-pos pos))

    (define/public (eof?)
      (= current-pos (string-length contents)))

    (define/public (get-contents) contents)
    (define/public (read-all)
      (read-string (- (string-length contents) current-pos)))))

(define (string-stream s)
  (new string-stream% [string s]))

;; TODO: file-stream%


;;; Monoids, represented explicitly.


;;; A parser is a function that takes:
;;; - an input stream
;;; - the extensible environment (a monoid of some sort)
;;;
;;; and either:
;;; - returns RESULT
;;; - or raises (fail HOW LOC ERRMSG)
;;; where HOW is either 'soft or 'hard.

(define (parse-string parser string)
  (let* ([instream (string-stream string)]
         [result (parser instream (void))])
    (values result (send instream read-all))))

(define (failed-soft? x) (matches? x `(fail soft ,_ ,_)))
(define (failed-hard? x) (matches? x `(fail hard ,_ ,_)))

(define (fail how loc errmsg)
  (raise `(fail ,how ,loc ,errmsg)))

(define (try p)
  (lambda (instream env)
    (let* ([orig-pos (get-pos instream)])
      (with-handlers ([failed-hard?
                       (match-lambda
                         [`(fail ,_ ,pos ,msg)
                           (restore instream orig-pos)
                           (fail 'soft pos msg)])])
        (p instream env)))))

(define ((catching p) instream env)
  (with-handlers ([failed-soft? (lambda (e) e)])
    (list 'ok (p instream env))))

(define ((fmap f . ks) instream env)
  (apply f (map (lambda (x) (x instream env)) ks)))

(define (>>= k . fs)
  (lambda (instream env)
    (foldl (lambda (f r) ((f r) instream env)) (k instream env) fs)))

(define/match (psum ps)
  [((list)) pzero]
  [((list p)) p]
  [((cons p ps))
    (>>= (catching p)
         (match-lambda
           [`(ok ,x) (const x)]
           [`(fail soft ,_ ,_) (psum ps)]))])

(define (choice . args) (psum args))

(define (pzero instream env)
  (fail 'soft (get-pos instream) "pzero called"))

(define (peof instream env)
  (unless (eof? instream)
    (fail 'soft (get-pos instream) "expected EOF")))

(define (string expect)
  (let ([len (string-length expect)])
    (if (= 0 len) (const (void))
      (lambda (instream env)
        (let ([got (read-string instream len)]
              [pos (get-pos instream)])
          (if (string=? got expect) expect
            (fail 'hard pos
              (string-append
                "expected " (repr expect)
                ", got" (repr got)))))))))


(displayln "loaded parse-monoid.rkt")

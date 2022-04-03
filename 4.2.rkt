#lang racket/base
(require "3.31.rkt")

; 1.8
; Ex.12

; behavior of letrec and let*
; in letrec, a binding has "vision" of all bindings in the same layer
#;(letrec ([always-zero (λ (n) (if (zero? n) n (always-zero (sub1 n))))])
  (always-zero 11))
#;(letrec ([my-odd?  (λ (n) (if (zero? n) #f (my-even? (sub1 n))))]
         [my-even? (λ (n) (if (zero? n) #t (my-odd? (sub1 n))))])
  (odd? 11))

; in let*, you can't call var-id in the corresponding rhs-expr
#;(let* ([always-zero (λ (n) (if (zero? n) n (always-zero (sub1 n))))])
  (always-zero 11))


; Answer to ex12:
; the environment of each binding is different. Every binding have access to
; the bindings above it.

; Ex.13:
(define-syntax-rule (my-cond-v0 [question-expr answer-expr] ...)
  (my-cond-v0-fn (list (cons (λ () question-expr) (λ () answer-expr)) ...)))

(define my-cond-v0-fn
  (λ (ls)
    (if (null? ls) (void)
        (if ((car (car ls))) ((cdr (car ls))) (my-cond-v0-fn (cdr ls))))))

; 1.9
(define-syntax my-let*
  (syntax-rules ()
    [(my-let* () body-expr)
     body-expr]
    [(my-let* ([id rhs-expr] binding ...) body-expr)
     (let ([id rhs-expr])
       (my-let* (binding ...) body-expr))]))

; Ex.14
(define-syntax my-and
  (syntax-rules ()
    [(my-and) #t]
    [(my-and b1 b2 ...)
     (if b1 (my-and b2 ...) #f)]))

(define-syntax my-or
  (syntax-rules ()
    [(my-or) #f]
    [(my-or b1 b2 ...)
     (if b1 #t (my-or b2 ...))]))

; 1.10
#;(define-syntax my-cond
  (syntax-rules (else)
    [(my-cond)
     (void)]
    [(my-cond [else answer-expr])
     answer-expr]
    [(my-cond [question-expr answer-expr] clause ...)
     (if question-expr answer-expr
         (my-cond clause ...))]))
; Ex.15
#;(define-syntax my-cond
  (syntax-rules (else =>)
    [(my-cond)
     (void)]
    [(my-cond [else answer-expr])
     answer-expr]
    [(my-cond [question-expr => proc-expr] clause ...)
     (let ([q question-expr])
       (if q
           (with-handlers ([exn:fail:contract? (λ (e) (printf "~s" e))])
             (proc-expr q))
           (my-cond clause ...)))]
    [(my-cond [question-expr answer-expr] clause ...)
     (if question-expr answer-expr
         (my-cond clause ...))]))
; tests
;> (my-cond
;   [(positive? -5) (error "doesn't get here")]
;   [(zero? -5) (error "doesn't get here, either")]
;   [(positive? 5) 'here])
;'here

;> (cond
;   [(member 2 '(1 2 3)) => (lambda (l) (map - l))])
;'(-2 -3)

;(my-cond
;   [(member 2 '(1 2 3)) => (lambda () (map - l))])
; returns an error

; Ex.16
(define-syntax my-cond
  (syntax-rules (else =>)
    [(my-cond)
     (void)]
    [(my-cond [else answer-expr])
     answer-expr]
    [(my-cond [question-expr] clause ...)
     (let ([q question-expr])
       (if q q (my-cond clause ...)))]
    [(my-cond [question-expr => proc-expr] clause ...)
     (let ([q question-expr])
       (if q
           (with-handlers ([exn:fail:contract? (λ (e) (printf "~s" e))])
             (proc-expr q))
           (my-cond clause ...)))]
    [(my-cond [question-expr answer-expr ...] clause ...)
     (if question-expr (begin answer-expr ...)
         (my-cond clause ...))]))

; some hotfix is applied


















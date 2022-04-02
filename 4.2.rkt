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































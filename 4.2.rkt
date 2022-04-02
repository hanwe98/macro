#lang racket/base
(require "./Desktop/racket-macro/Culpepper/3.31.rkt")

; 1.8
; Ex.12
(define-syntax-rule (my-let* ([var-id rhs-expr] ...) body-expr)
  (my-let ([var-id #f] ...)
          (begin
            (set! var-id rhs-expr) ...
            body-expr)))

#;(my-let* ([odd?  (λ (n) (if (zero? n) #f (even? (sub1 n))))]
       [even? (λ (n) (if (zero? n) #t (odd? (sub1 n))))])
  (odd? 11))

#;(letrec ([my-odd?  (λ (n) (if (zero? n) #f (my-even? (sub1 n))))]
         [my-even? (λ (n) (if (zero? n) #t (my-odd? (sub1 n))))])
  (odd? 11))

(letrec ([always-zero (λ (n) (if (zero? n) n (always-zero (sub1 n))))])
  (always-zero 11))

; in let*, you can't call var-id in the corresponding rhs-expr
#;(let* ([always-zero (λ (n) (if (zero? n) n (always-zero (sub1 n))))])
  (always-zero 11))



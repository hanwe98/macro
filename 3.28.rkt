#lang racket/base

(define ls null)
(define-syntax-rule (assert expr)
  (unless expr
    (error 'assert "assertion failed: ~s" (quote expr))))

#;(assert (>= (length ls) 1))
(define assert-fn
  (λ (expr)
    (unless expr
      (error 'assert "assertion failed: ~s" expr))))
#;(assert-fn (>= (length ls) 1))

; Ex.1
(define-syntax-rule (noisy-v1 expr)
  (begin
    (fprintf (current-output-port)
             "evaluating ~s\n"
             (quote expr))
    expr))

; Ex.2
(define-syntax-rule (noisy-v2 expr)
  (begin
    (fprintf (current-output-port)
             "evaluating ~s\n"
             (quote expr))
    (begin0
      expr
      (display "done\n"))))




























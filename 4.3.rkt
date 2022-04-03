#lang racket/base

; this version is bad because val can be evaluated many times in the third case
(define-syntax my-case-v0
  (syntax-rules (else)
    [(my-case-v0 val)
     (void)]
    [(my-case-v0 val [else result-expr])
     result-expr]
    [(my-case-v0 val [(datum ...) result-expr] clause ...)
     (if (member val '(datum ...))
         result-expr
         (my-case-v0 val clause ...))]))

; to fix the above issue, we use let-binding to compute val, then send it to the helper macro
(define-syntax-rule (my-case val clause ...)
  (let ([v val])
    (my-case* v clause ...)))
(define-syntax my-case*
  (syntax-rules (else)
    [(my-case-v0 val)
     (void)]
    [(my-case-v0 val [else result-expr])
     result-expr]
    [(my-case-v0 val [(datum ...) result-expr] clause ...)
     (if (member val '(datum ...))
         result-expr
         (my-case-v0 val clause ...))]))

; Ex.17
(define-syntax-rule (minimatch1 var-expr mm-pattern result-expr)
  (let ([v var-expr])
    (minimatch1* v mm-pattern result-expr)))
(define-syntax minimatch1*
  (syntax-rules (cons quote)
    [(minimatch1 val-pv (quote x) result-expr)
     3]
    [(minimatch1 val-pv (cons x y) result-expr)
     2]
    [(minimatch1 val-pv var-id result-expr)
     1]))






























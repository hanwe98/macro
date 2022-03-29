#lang racket/base

(define-syntax-rule (iflet x e1 e2 e3)
  (let ([thunk3 (lambda () e3)])
    (let ([x e1])
      (if x e2 (thunk3)))))

(define-syntax-rule (iflet-param x e1 e2 e3)
  (parameterize ([thunk3 (lambda () e3)])
    (parameterize ([x e1])
      (if x e2 (thunk3)))))

; Ex.3 some more thoughts
;(define location (make-parameter "here"))
;(let ([get (parameterize ([location "with a fox"])
;             (lambda () (location)))])
;  (get))
;
;(let ([location "here"])
;  (let ([get (let ([location "with a fox"])
;               (lambda () location))])
;    (get)))

; Ex.4
(define-syntax-rule (forever expr)
  (begin
    expr
    (printf "~s\n" (quote expr))
    (forever expr)))

; Ex.5































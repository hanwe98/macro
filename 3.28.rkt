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
    (printf "evaluating ~s\n" (quote expr))
    expr))

; Ex.2
(define-syntax-rule (noisy-v2 expr)
  (begin
    (printf "evaluating ~s\n" (quote expr))
    (begin0
      expr
      (display "done\n"))))

; 1.4
; Ex.3
(define-syntax-rule (andlet1 var e1 e2)
  (let ([var e1])
    (if var e2 #f)))

(define-syntax-rule (iflet var e1 e2 e3)
  (let ([var e1])
    (if var e2 e3))
  #;(let ([tmp e1])
    (if tmp
        (let ([var tmp]) e2)
        e3)))

(define alist '((1 . apple) (2 . pear)))
(equal? (iflet x (assoc 1 alist) (cdr x) 'none) 'apple)
(equal? (let ([x 'plum]) (iflet x (assoc 3 alist) (cdr x) x)) 'plum)

;(eval (if x e2 (thunk3)) '(thunk3 x))
; --> (eval x '(thunk3 x)) = (eval e1 '(thunk3 x)) = v1 
;
; --> (eval e2 '(thunk3 x)) = e2[x <- v1]
;
; --> (eval (thunk3) '(thunk3 x))
;   = (eval ((lambda () e3)) '(thunk3 x))
;   = (eval e3 '(thunk3 x))
;   = e3[x <- v1]























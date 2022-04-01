#lang racket/base

; 1.7
(define-syntax-rule (capture-output e ...)
  (print-input (list (lambda () e) ...))
  #;(capture-output-fun (λ () e ... (void))))  
 
; capture-output-fun : (-> Any) -> String
(define (capture-output-fun thunk)
  (let ([out (open-output-string)])
    (parameterize ((current-output-port out))
      (thunk)
      (get-output-string out))))

(define-syntax-rule (print-input i)
  (printf "~s" (quote i)))

; Fred
; 1. (list exmaple) ellipsis not only replicate the pattern variable,
; not also imitate the operation on the pattern variable?
; 2. (void example) Is there a begin, i.e., (begin e ...)?
; 3. 

; Ex.10
; check with Fred
; What is an alternative way besides using a helper function?
(define-syntax-rule (my-and e1 ...)
  (my-and-fn (list (λ () e1) ...)))

(define my-and-fn
  (λ (ls)
    (cond
      [(null? ls) #t]
      [else
       (if ((car ls)) (my-and-fn (cdr ls)) #f)])))

(define-syntax-rule (my-or e1 ...)
  (my-or-fn (list (λ () e1) ...)))

(define my-or-fn
  (λ (ls)
    (cond
      [(null? ls) #f]
      [else
       (if ((car ls)) #t (my-or-fn (cdr ls)))])))

; 1.8
; demonstration of pattern match
(define-syntax-rule (my-typle [(t1 t2)])
  t1)
;> (my-typle 1 2)
; my-typle: use does not match pattern: (my-typle ((t1 t2))) in: (my-typle 1 2)
;> (my-typle (1 2))
; my-typle: use does not match pattern: (my-typle ((t1 t2))) in: (my-typle (1 2))
;> (my-typle ((1 2)))
;1

(define-syntax-rule (my-let ([var-id rhs-expr] ...) body-expr)
  ((λ (var-id ...) body-expr) rhs-expr ...))

(define-syntax-rule (my-letrec ([var-id rhs-expr] ...) body-expr)
  (my-let ([var-id #f] ...)
          (set! var-id rhs-expr) ... ; why? Fred
          body-expr))
; Note:
; 1. 
; Assumption to set! in my-letrec: Fred
; rhs-expr calls var-id in its body. So, var-ids inside rhs-expr are unbounded if not given a initial value.

; 2. Ellipsis imitate the previous term in the following way:
; - If a subterm is a pattern varaible with ellipsis in the definition, iterate through the given values
; - Otherwise, copy the exact content of the previous term.
(define-syntax-rule (ellipsis-example (var-num ...) fix-num)
  (begin (printf "~s\n" (+ var-num fix-num)) ...))


; Ex.11
; syntax-error
; 1.
;(my-let ([1 0]) 1)
; λ: not an identifier, identifier with default, or keyword in: 1
; 2. 
; ... Can't think of any  Fred

; run-time error
; (my-let ([x 3]) (string-append x "hi"))

; misuse with no error?  Fred
;(let ([x 0])
;  (my-let ([x 1] [y x]) y))
; Expect: 1
; Return: 0



















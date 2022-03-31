#lang racket/base
(require rackunit)

#;(define-syntax-rule (iflet x e1 e2 e3)
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
#;(define-syntax-rule (forever expr)
  (begin
    expr
    (printf "~s\n" (quote expr))
    (forever expr)))

; Ex.5
#;(define-syntax-rule (handle e1 e2)
  (with-handlers ([exn? (λ (e) e2)])
    e1))

; 1.6
; Ex.6
(define-syntax-rule (forever expr)
  (forever-fn (λ () expr)))
(define forever-fn
  (λ (thunk)
    (begin
      (thunk)
      (forever-fn thunk))))

(define-syntax-rule (handle e1 e2)
  (handle-fn (λ () e1) (λ () e2)) 
  #;(with-handlers ([exn? (λ (e) e2)])
    e1))
(define handle-fn
  (λ (t1 t2)
    (with-handlers ([exn? (λ (e) (t2))])
      (t1))))

;(equal? (handle 5 6) 5)
;(equal? (handle (/ 1 0) 'whoops) 'whoops)

; Ex.7
(define-syntax-rule (andlet1 var e1 e2)
  (andlet1-fn (λ () e1) (λ (var) e2))
  #;(let ([var e1])
    (if var e2 #f)))
(define andlet1-fn
  (λ (t1 t2)
    (let ([var (t1)])
      (if var (t2 var) #f))))
;(andlet1 x #t (not (not x)))
;(andlet1 x #t (not x))
;(andlet1 y #t (and y (andlet1 x #t (not x))))
;(andlet1 y #t (or y (andlet1 x #t #t)))

(define-syntax-rule (iflet x e1 e2 e3)
  #;(let ([tmp e1])
    (if tmp
        (let ([x tmp])
          e2)
        e3))
  (iflet-fn (λ () e1) (λ (x) e2) (λ () e3))
  #;(let ([thunk3 (lambda () e3)])
    (let ([x e1])
      (if x e2 (thunk3)))))
(define iflet-fn
  (λ (t1 t2 t3)
    (let ([var (t1)])
      (if var (t2 var) (t3)))))

;(define alist '((1 . apple) (2 . pear)))
;(equal? (iflet x (assoc 1 alist) (cdr x) 'none) 'apple)
;(equal? (let ([x 'plum]) (iflet x (assoc 3 alist) (cdr x) x)) 'plum)

; Ex.8
(define-syntax-rule (test name actual-expr expected-expr)
  (test-fn name (λ () actual-expr) (λ () expected-expr)))

(define test-fn 
  (λ (name actual-thunk expected-thunk)
    (with-handlers ([exn? (λ (e) (printf "test ~s failed\n" name))])
      (let ([actual (actual-thunk)]
            [expected (expected-thunk)])
        (if (equal? actual expected) (void) (printf "test ~s failed\n" name))))))
        
;(test "Fruit test" "apple" "pear")
;(test "Fruit test" "apple" "apple")
;(test "Fruit test" "pear" "apple")
;(test "Fruit test" "pear" (/ 1 0))

; Ex.9 (check with Fred)
; If the task is to control the flow of evaluation, leave it to users.
; If the task involves the information from the input literal, then use macro



















#lang racket
(require rackunit)

; Author: Kim Arre (karre@calpoly.edu)
; CPE 430 - Spring 2016

;; an Environment is a hash table mapping symbols to values
;(define-type Env (HashTable Symbol Value))
(define empty-env (hash))

;(define-type ExprC (U NumC IdC BinopC AppC ifleq0C FundefC))
(struct NumC (n) #:transparent)
(struct IdC (x) #:transparent)
(struct AppC (fun args) #:transparent) ; use of a function (call)
(struct ifleq0C (n then else) #:transparent)
(struct FundefC (name args body) #:transparent)
(struct BinopC (name left right) #:transparent)
(struct LamC (param body) #:transparent) ; Symbol ExprC

;; Value that represents the result of evaluation:
; (define-type Value (U NumV CloV))
(struct NumV (n) #:transparent) ; Real
(struct CloV (param body env) #:transparent) ; Symbol ExprC Env

(define Operations (hash
                    'plus +
                    'mult *
                    'minus -
                    'divide /))

(define funs (list
              (FundefC 'add-three (list 'y) (BinopC 'plus (IdC 'y) (NumC 3))) 
              (FundefC 'double-plus-one (list 'x) (BinopC 'plus (BinopC 'mult (NumC 2) (IdC 'x)) (NumC 1)))
              (FundefC 'add-two-nums (list 'x 'y) (BinopC 'plus (IdC 'x) (IdC 'y)))
              (FundefC 'main (list) (NumC 7))
              ))

;(: interp-fns ((Listof FundefC) -> Real))
(define (interp-fns complete-funs funs)
  (cond
    [(string=? (symbol->string (FundefC-name (first funs))) "main")
     (interp (FundefC-body (first funs)) complete-funs)]
    [else (interp-fns complete-funs (rest funs))]))

(define reserved-ops (list '\ '* '+ '- 'ifleq0))

; Determine if a symbol being used is part of our language already
(define (isReserved? x)
  (cond
    [(equal? '/ x) (error "DFLY: Cannot use reserved function name.")]
    [(equal? '* x) (error "DFLY: Cannot use reserved function name.")]
    [(equal? '+ x) (error "DFLY: Cannot use reserved function name.")]
    [(equal? '- x) (error "DFLY: Cannot use reserved function name.")]
    [(equal? 'ifleq0 x) (error "DFLY: Cannot use reserved function name.")]
    [else #false]))


; =======================================
;                Parse
; =======================================
;; Parser - Converts a Sexp to an ExprC
;(: parse (Sexp -> ExprC))
(define (parse sexp)
  (match sexp
    [(? real? n) (NumC n)]
    [(? symbol? x)
     (cond
       [(not (isReserved? x))
        (IdC x)])]
    [(list 'ifleq0 test then el) (ifleq0C (parse test) (parse then) (parse el))]
    [(list '+ l r) (BinopC 'plus (parse l) (parse r))]
    [(list '- l r) (BinopC 'minus (parse l) (parse r))]
    [(list '* l r) (BinopC 'mult (parse l) (parse r))]
    [(list '/ l r) (BinopC 'divide (parse l) (parse r))]
    [(list (? symbol? fun) arg ...)
     (cond
       [(not (isReserved? fun))
            (AppC (parse fun) (map parse arg))])]
    [(list 'lam (list (? symbol? param)) body)
     (LamC param (parse body))]
    [other (error "DFLY: Oh noes, input not well-formed")]))

(check-exn #px"DFLY: Oh noes, input not well-formed" (λ () (parse '(16))))
(check-exn #px"DFLY: Cannot use reserved function name." (λ () (parse '(/ 3 4 5))))

(check-exn #px"DFLY: Cannot use reserved function name." (λ () (parse '(+ cat dog poop))))
(check-exn #px"DFLY: Cannot use reserved function name." (λ () (parse '(* 3))))
(check-exn #px"DFLY: Cannot use reserved function name." (λ () (parse '(- 3 4 5))))
(check-exn #px"DFLY: Cannot use reserved function name." (λ () (parse '(ifleq0 3 cat))))
(check-exn #px"DFLY: Cannot use reserved function name." (λ () (parse '(+ / 3))))

(check-equal? (parse '{+ 1 2}) (BinopC 'plus (NumC 1) (NumC 2)))
(check-equal? (parse '{* 2 3}) (BinopC 'mult (NumC 2) (NumC 3)))
(check-equal? (parse '{- 4 2}) (BinopC 'minus (NumC 4) (NumC 2)))
(check-equal? (parse '{/ 6 3}) (BinopC 'divide (NumC 6) (NumC 3)))
(check-equal? (parse '3) (NumC 3))
(check-equal? (parse '{g {+ 3 x}}) (AppC (IdC 'g) (list (BinopC 'plus (NumC 3) (IdC 'x)))))
(check-equal? (parse '{ifleq0 {- 0 x} 0 1}) (ifleq0C (BinopC 'minus (NumC 0) (IdC 'x)) (NumC 0) (NumC 1)))

;; parse an s-expression into a FundefC
;(: parse-fundef (Sexp -> FundefC))
(define (parse-fundef s)
  (match s
    [(list 'func (? symbol? name) (list (? symbol? args) ...) body)
     (cond
       [(not (isReserved? name))
        (FundefC name args (parse body))])]
    [other
     (error "DFLY: Oh noes, input to parse-fundef not well-formed")]))

(check-equal? (parse-fundef '{func g {x} {* x 9}}) (FundefC 'g (list 'x) (BinopC 'mult (IdC 'x) (NumC 9))))
(check-exn #px"DFLY: Oh noes, input to parse-fundef not well-formed" (λ () (parse-fundef '{func + () () 13})))
(check-exn #px"DFLY: Cannot use reserved function name" (λ () (parse-fundef '{func + () 13})))

;; Turns a list of s-expressions to a list of FundefC's
;(: parse-prog ((Listof Sexp) -> (Listof FundefC)))
(define (parse-prog s)
   (map parse-fundef s))

(define herpderpExp (FundefC 'herpderp (list 'a 'b) (BinopC 'plus (IdC 'a) (IdC 'b))))
(define meowExp (FundefC 'meow (list 'x 'y) (BinopC 'plus (BinopC 'mult (IdC 'x) (IdC 'y)) (NumC 1))))

(check-equal? (parse-prog (list
                           '{func herpderp {a b} {+ a b}}
                           '{func meow {x y} {+ {* x y} 1}}))
              (list herpderpExp meowExp))


; ======================================= TODO: update binop with env
;                Interp
; =======================================

;; Interpret an expression in a given environment
;(: interp (ExprC Env -> Value))
(define (interp expr env)
  (match expr
    [(NumC n) (NumV n)]
    [(LamC param body) (CloV param body env)]
    [(IdC i) (hash-ref env i)]
    [(BinopC name l r)
     (define rVal (interp r env))
     (cond
       [(and (= 0 (NumV-n rVal)) (equal? name 'divide)) (error "DFLY: Cannot divide by 0.")]
       [else (NumV ((hash-ref Operations name) (NumV-n (interp l env)) (NumV-n (interp r env))))])]

    #;( ; This was our AppC for A2
       [(AppC f a) (local ([define fd (get-fundef f funs)])
                  (interp (foldl (λ (var val result)
                                   (subst var val result))
                                 (FundefC-body fd) ; initial value
                                 (FundefC-args fd) ; vars to be sub'd
                                 (map
                                  (λ (x) (NumC (interp x funs))) ; vals to sub in
                                  a))
                          
                          funs))]
       )
    
    ; Instead of subst, use closure hash
    ; This was very close, all the pieces are there, just confusing.
    ; Separate into two helper functions for the map and foldl
    ; Write good test cases to make sure each is doing what I think,
    [(AppC fun args)
     (match (interp fun env)
       [(CloV params body clo-env)
        (interp (foldl (λ (var val result)
                         (hash-set clo-env param
                          (map (λ (x) ((interp x env)))
                               args))
                         body)])]

    [(ifleq0C test then el)
     (cond
       [(<= (interp test funs) 0) (interp then funs)]
       [else (interp el funs)])]))


(define (

;; Test cases for Interp
(check-equal? (interp (parse '{+ 3 4}) empty-env) (NumV 7))
(check-equal? (interp (parse '{* 3 {+ 4 5}}) empty-env) (NumV 27))

(check-equal? (interp (parse '{v {+ 3 4}})
                      (hash
                       'z (CloV 'q (parse '{+ q 1}) empty-env)
                       'v (CloV 'q (parse '{+ q -1}) empty-env)))
              (NumV 6))

(check-exn #px"no value found for key"
           (λ ()
             (interp (parse '{calls-b 3})
                     (hash
                      'calls-b (CloV 'x (parse '{b 24}) empty-env)
                      'b       (CloV 'y (parse '{+ x y}) empty-env)))))

#;( ;old test cases for interp
   ;(check-exn #px"DFLY: Interp should not have encountered an IdC on its own. Undefined variable."
   ;           (λ () (interp (IdC 'x) funs)))
   (check-equal? (interp (AppC 'add-three (list (NumC 4))) funs) 7)
   (check-equal? (interp (NumC 6) funs) 6)
   (check-equal? (interp (BinopC 'plus (NumC 2) (NumC 3)) funs) 5)
   (check-equal? (interp (BinopC 'mult (NumC 4) (NumC 2)) funs) 8)
   (check-equal? (interp (ifleq0C (BinopC 'minus (NumC 0) (NumC 1)) (NumC 0) (NumC 1)) funs) 0)
   (check-equal? (interp (ifleq0C (BinopC 'minus (NumC 1) (NumC 0)) (NumC 0) (NumC 1)) funs) 1)
)

;; Evaluates an Sexp by calling parse and interp - Interpret main
;(: top-interp ((Listof Sexp) -> Real))
(define (top-interp fun-sexps)
  (interp-fns (parse-prog fun-sexps) (parse-prog fun-sexps)))

(check-equal? (top-interp
               '{{func add-two {x} {+ 2 x}}
                 {func main {} {add-two 4}}}) 6)

(check-equal? (top-interp
               '{{func add-together {x y} {+ y x}}
                 {func main {} {add-together 4 2}}}) 6)

(check-equal? (top-interp
               '{{func isLessThan1 {x} {ifleq0 x 1 0}}
                 {func main {} {isLessThan1 3}}}) 0)

(check-exn #px"DFLY: Cannot divide by 0" (λ () (top-interp
                                                '{{func divideByZero {x} (/ x 0)}
                                                  (func main () (divideByZero 2))})))

(check-exn #px"DFLY: Cannot divide by 0" (λ () (top-interp
                                                '{{func ignoreit (x) (+ 3 4)}
                                                  (func main () (ignoreit (/ 1 0)))})))


; nested functions are just lamc's in the body of another lamc
; {lam {x} {lam {y} {+ x y}}}
; --->
; (hash 'g (cloV 'x (parse '{lam {y} {+ x y}}}) empty-env))
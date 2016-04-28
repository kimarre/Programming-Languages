#lang racket
(require rackunit)

; Author: Kim Arre (karre@calpoly.edu)
; CPE 430 - Spring 2016
; =====================================

;; an Environment is a hash table mapping symbols to values
;(define-type Env (HashTable Symbol Value))
(define empty-env (hash))

;(define-type ExprC (U NumC IdC BinopC AppC ifleq0C FundefC))
(struct NumC (n) #:transparent)
(struct IdC (x) #:transparent)
(struct AppC (fun args) #:transparent) ; use of a function (call)
(struct ifC (test then else) #:transparent)
(struct BinopC (name left right) #:transparent)
(struct LamC (param body) #:transparent) ; Symbol ExprC
(struct BooleanC (b) #:transparent)


;; Value that represents the result of evaluation:
; (define-type Value (U NumV CloV BooleanV))
(struct NumV (n) #:transparent) ; Real
(struct CloV (params body env) #:transparent) ; Symbol ExprC Env
(struct BooleanV (val) #:transparent)

;; Operations and uses for interp to look up
(define Operations (hash
                    'plus +
                    'mult *
                    'minus -
                    'divide /
                    'eq eq?
                    '<= <=))

(define reserved-ops (list '\ '* '+ '- 'if 'eq '<= 'with 'true 'false 'lam))

; Determine if a symbol being used is part of our language already
(define (isReserved? x)
  (cond
    [(equal? '/ x) (error "DFLY: Cannot use reserved function name.")]
    [(equal? '* x) (error "DFLY: Cannot use reserved function name.")]
    [(equal? '+ x) (error "DFLY: Cannot use reserved function name.")]
    [(equal? '- x) (error "DFLY: Cannot use reserved function name.")]
    [(equal? 'if x) (error "DFLY: Cannot use reserved function name.")]
    [else #false]))

(define (isBooleanVal x)
  (cond
    [(equal? x 'true) #true]
    [(equal? x 'false) #true]
    [else #false]))

(check-equal? (isBooleanVal 'true) #true)
(check-equal? (isBooleanVal 'false) #true)
(check-equal? (isBooleanVal 'pizza) #false)

; =======================================
;                Parse
; =======================================
;; Parser - Converts a Sexp to an ExprC
;(: parse (Sexp -> ExprC))
(define (parse sexp)
  (match sexp
    [(? real? n) (NumC n)]
    [(list 'lam (list (? symbol? params) ...) body)
     (LamC params (parse body))]
    [(? symbol? x)
     (cond
       [(isBooleanVal x) (BooleanC x)]
       [(not (isReserved? x)) (IdC x)])]
    [(list 'if test then el)
     ;(cond
       ;(define testResult (parse test))
       ;[(boolean? testResult) (ifC (parse test) (parse then) (parse el))]
       ;[else (error "DFLY: Test for the 'if' must be a boolean")]
       (ifC (parse test) (parse then) (parse el))]
    [(list '+ l r) (BinopC 'plus (parse l) (parse r))]
    [(list '- l r) (BinopC 'minus (parse l) (parse r))]
    [(list '* l r) (BinopC 'mult (parse l) (parse r))]
    [(list '/ l r) (BinopC 'divide (parse l) (parse r))]
    [(list 'eq? l r) (BinopC 'eq (parse l) (parse r))]
    ((list '<= l r) (BinopC '<= (parse l) (parse r)))
    [(list (? symbol? fun) arg ...)
     (cond
       [(not (isReserved? fun))
            (AppC (parse fun) (map parse arg))])]
    ;[(list 'with (list params ...) 
    [other (error "DFLY: Oh noes, input not well-formed")]))

(check-equal? (parse '{lam {a b} true}) (LamC (list 'a 'b) (BooleanC 'true)))
(check-equal? (parse '{lam {a b} 3}) (LamC (list 'a 'b) (NumC 3)))
(check-equal? (parse '{lam {x y} {- x y}}) (LamC (list 'x 'y) (BinopC 'minus (IdC 'x) (IdC 'y))))
(check-equal? (parse '{if {eq? 2 2} 1 0}) (ifC (BinopC 'eq (NumC 2) (NumC 2)) (NumC 1) (NumC 0)))
(check-equal? (parse '{if true 1 0}) (ifC (BooleanC 'true) (NumC 1) (NumC 0)))

(check-exn #px"DFLY: Oh noes, input not well-formed" (λ () (parse '(16))))
(check-exn #px"DFLY: Cannot use reserved function name." (λ () (parse '(/ 3 4 5))))
(check-exn #px"DFLY: Cannot use reserved function name." (λ () (parse '(+ cat dog poop))))
(check-exn #px"DFLY: Cannot use reserved function name." (λ () (parse '(* 3))))
(check-exn #px"DFLY: Cannot use reserved function name." (λ () (parse '(- 3 4 5))))
(check-exn #px"DFLY: Cannot use reserved function name." (λ () (parse '(+ / 3))))
(check-exn #px"DFLY: Cannot use reserved function name." (λ () (parse '(if / 3))))

(check-equal? (parse '{+ 1 2}) (BinopC 'plus (NumC 1) (NumC 2)))
(check-equal? (parse '{* 2 3}) (BinopC 'mult (NumC 2) (NumC 3)))
(check-equal? (parse '{- 4 2}) (BinopC 'minus (NumC 4) (NumC 2)))
(check-equal? (parse '{/ 6 3}) (BinopC 'divide (NumC 6) (NumC 3)))
(check-equal? (parse '{eq? 6 6}) (BinopC 'eq (NumC 6) (NumC 6)))
(check-equal? (parse '{<= 4 5}) (BinopC '<= (NumC 4) (NumC 5)))
(check-equal? (parse '3) (NumC 3))
(check-equal? (parse '{g {+ 3 x}}) (AppC (IdC 'g) (list (BinopC 'plus (NumC 3) (IdC 'x)))))
;(check-equal? (parse '{if {- 0 x} 0 1}) (ifC (BinopC 'minus (NumC 0) (IdC 'x)) (NumC 0) (NumC 1)))


; =======================================
;                Interp
; =======================================

(define (isBoolean x)
  (match x
    [(BooleanC x) #true]
    [(BooleanV x) #true]
    [other #false]))

(define (interp-args args env)
  (map (λ (x) (interp x env)) args))

;; Interpret an expression in a given environment
;(: interp (ExprC Env -> Value))
(define (interp expr env)
  (match expr
    [(NumC n) (NumV n)]
    [(LamC params body) (CloV params body env)]
    [(IdC i) (hash-ref env i (λ () (error "DFLY: no value found for key" i)))]
    [(ifC test then el)
     (define testResult (interp test env))
     (cond
       [(not (isBoolean testResult)) (error "DFLY: if's test must be a boolean. Got ~e" testResult)]
       [else
        (cond
          [testResult (interp then env)]
          [else (interp el env)])])]
    [(BinopC name l r)
     (define rVal (interp r env))
     (define lVal (interp l env))
     (cond
       [(and (= 0 (NumV-n rVal)) (equal? name 'divide)) (error "DFLY: Cannot divide by 0")]
       [(equal? name '<=)
        (cond
          [(<= (NumV-n lVal) (NumV-n rVal)) (BooleanV 'true)]
          [else (BooleanV 'false)])]
       [(equal? name 'eq)
        (cond
          [(eq? (NumV-n lVal) (NumV-n rVal)) (BooleanV 'true)]
          [else (BooleanV 'false)])]
       [else (NumV ((hash-ref Operations name) (NumV-n (interp l env)) (NumV-n (interp r env))))])]
    [(BooleanC bool) (BooleanV bool)]
    [(AppC fun args)
     (match (interp fun env)
       [(CloV params body clo-env)
        (interp body (foldl  ; returns a new env containing the param/arg pairs
                      (λ (param arg new-env)
                        (hash-set new-env param arg))
                      clo-env
                      params
                      (interp-args args env)))])]))


;; Test cases for Interp
;(check-equal? (interp (ifC (BooleanC 'false) (BooleanC 'true) 1) empty-env) (NumV 1)) 
;(check-equal? (interp (parse '{if false 0 1}) empty-env) (NumV 1))

(check-equal? (interp (ifC (BooleanC 'true) (NumC 1) (NumC 0)) empty-env) (NumV 1))

;(check-equal? (interp (ifC (BooleanC 'false) (NumC 1) (NumC 0)) empty-env) (NumV 0))
;(check-equal? (parse '{if true 1 0}) (ifC (BooleanC 'true) (NumC 1) (NumC 0)))

(check-equal? (interp (parse '{<= 3 4}) empty-env) (BooleanV 'true))
(check-equal? (interp (parse '{<= 9 4}) empty-env) (BooleanV 'false))

(check-equal? (interp (parse '{eq? 3 4}) empty-env) (BooleanV 'false))
(check-equal? (interp (parse '{eq? 5 5}) empty-env) (BooleanV 'true))

(check-equal? (interp (parse '{+ 3 4}) empty-env) (NumV 7))
(check-equal? (interp (parse '{* 3 {+ 4 5}}) empty-env) (NumV 27))

(check-equal? (interp (parse '{v {+ 3 4}})
                      (hash
                       'z (CloV (list 'q) (parse '{+ q 1}) empty-env)
                       'v (CloV (list 'q) (parse '{+ q -1}) empty-env)))
              (NumV 6))

(check-equal? (interp (parse '{lam {a b} {+ a b}}) empty-env)
              (CloV (list 'a 'b) (BinopC 'plus (IdC 'a) (IdC 'b)) empty-env))

(check-exn #px"DFLY: Cannot divide by 0"
           (λ () (interp (parse '{/ 1 0}) empty-env)))

(check-exn #px"DFLY: no value found for key"
           (λ () (interp (parse '{calls-b 3})
                         (hash
                          'calls-b (CloV (list 'x) (parse '{b 24}) empty-env)
                          'b       (CloV (list 'y) (parse '{+ x y}) empty-env)))))

;; Turn a DFLY3 value to a string
; (: serialize (DFLY3 -> String))
(define (serialize x)
  (match x
    [(NumV n) (~v n)]
    [(CloV params body env) "<#procedure>"]
    [(BooleanV bool)
     (cond
       [(equal? bool 'true) "true"]
       [(equal? bool 'false) "false"])]))

(check-equal? (serialize (NumV 1)) "1")
(check-equal? (serialize (CloV (list 'x) (parse '{+ 1 2}) empty-env)) "<#procedure>")
(check-equal? (serialize (BooleanV 'true)) "true")
(check-equal? (serialize (BooleanV 'false)) "false")

;; Combine parsing and evaluation
; (: top-eval (sexp -> string)
(define (top-eval s)
  (serialize (interp (parse s) empty-env)))

;(check-equal? (top-eval '{lam {+ 2 3}) "5")
(check-equal? (top-eval 'true) "true")

;(check-equal? (top-eval '{{+ a 3} 2}) "5")

;lam {a b} {b {+ 2 3}}




; parse {with 4} is valid (same for interp)
; binop as function is valid for parser but not interpreter

; nested functions are just lamc's in the body of another lamc
; {lam {x} {lam {y} {+ x y}}}
; --->
; (hash 'g (cloV 'x (parse '{lam {y} {+ x y}}}) empty-env))
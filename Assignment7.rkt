#lang racket
(require rackunit)
(require racket/hash)

; Author: Kim Arre (karre@calpoly.edu)
; CPE 430 - Spring 2016
#;(
   =====================================
   After done adding strings, make a copy for the next assignment
   
   =====================================
)
; params - x y
; args   - 4 3

; 26 total tests
; 14 failing


;(define-type ExprC (U NumC IdC BinopC AppC ifC FundefC))
(struct NumC (n) #:transparent)
(struct StringC (s) #:transparent)
(struct IdC (x) #:transparent)
(struct AppC (fun args) #:transparent) ; use of a function (call)
(struct ifC (test then else) #:transparent)
(struct BinopC (name left right) #:transparent)
(struct LamC (param body) #:transparent) ; Symbol ExprC

;; Value that represents the result of evaluation
; (define-type Value (U NumV CloV BooleanV))
(struct NumV (n) #:transparent) ; Real
(struct CloV (params body env) #:transparent) ; Symbol ExprC Env
(struct BooleanV (val) #:transparent)
(struct StringV (s) #:transparent)

;; an Environment is a hash table mapping symbols to values
;(define-type Env (HashTable Symbol Value))
(define empty-env (hash))

(define top-env (hash
                 'true (BooleanV 'true)
                 'false (BooleanV 'false)
                 '+ (CloV (list 'l 'r) (BinopC 'plus (IdC 'l) (IdC 'r)) empty-env)
                 '- (CloV (list 'l 'r) (BinopC 'minus (IdC 'l) (IdC 'r)) empty-env)
                 '/ (CloV (list 'l 'r) (BinopC 'divide (IdC 'l) (IdC 'r)) empty-env)
                 '* (CloV (list 'l 'r) (BinopC 'mult (IdC 'l) (IdC 'r)) empty-env)))

;; Operations and uses for interp to look up
(define Operations (hash
                    'plus +
                    'mult *
                    'minus -
                    'divide /
                    'eq eq?
                    '<= <=))

(define reserved-ops (list '\ '* '- 'if 'eq '<= 'with 'true 'false 'lam))

; Determine if a symbol being used is part of our language already
(define (isReserved? x)
  (cond
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
    [(list 'with (list params '= args) ... body)
     (cond
       [(check-duplicates params) (error "DFLY: Cannot have duplicate params")]
       [else 
        (AppC (LamC params (parse body))
              (map (λ (arg) (parse arg)) args))])]
    [(? real? n) (NumC n)]
    [(? string? s) (StringC s)]
    [(list 'lam (list params ...) body)
     (cond
       [(check-duplicates params) (error "DFLY: Cannot have two parameters with the same name")]
       [else (LamC params (parse body))])]
    [(? symbol? x)
     (cond
       [(not (isReserved? x)) (IdC x)])]
    [(list 'if test then el)
       (ifC (parse test) (parse then) (parse el))]
    [(list 'eq? l r) (BinopC 'eq (parse l) (parse r))]
    ((list '<= l r) (BinopC '<= (parse l) (parse r)))
    [(list fun arg ...)
     (cond
       [(not (isReserved? fun))
            (AppC (parse fun) (map parse arg))])]
    [other (error "DFLY: Oh noes, input not well-formed")]))

(check-equal? (parse '{with {z = 9}
                            {y = 10}
                            {+ z y}})
              (AppC (LamC (list 'z 'y) (AppC (IdC '+) (list (IdC 'z) (IdC 'y)))) (list (NumC 9) (NumC 10))))

(check-exn #px"DFLY: Cannot have two parameters with the same name" (λ () (parse '(lam (x x) 3))))

(check-equal? (parse '{lam {a b} 3}) (LamC (list 'a 'b) (NumC 3)))
(check-equal? (parse '{lam {x y} {- x y}}) (LamC (list 'x 'y) (AppC (IdC '-) (list (IdC 'x) (IdC 'y)))))
(check-equal? (parse '{if {eq? 2 2} 1 0}) (ifC (BinopC 'eq (NumC 2) (NumC 2)) (NumC 1) (NumC 0)))

(check-exn #px"DFLY: Oh noes, input not well-formed" (λ () (parse '(#true))))
(check-exn #px"DFLY: Cannot use reserved function name." (λ () (parse '(if / 3))))
(check-exn #px"DFLY: Cannot have duplicate params" (λ () (parse '(with (z = (lam () 3))
                                           (z = 9)
                                           (z)))))

(check-exn #px"DFLY: " (λ () (parse '(+ + +))))

(check-equal? (parse "meow") (StringC "meow"))

(check-equal? (parse '{+ 1 2}) (AppC (IdC '+) (list (NumC 1) (NumC 2))))
(check-equal? (parse '{* 2 3}) (AppC (IdC '*) (list (NumC 2) (NumC 3))))
(check-equal? (parse '{- 4 2}) (AppC (IdC '-) (list (NumC 4) (NumC 2))))
(check-equal? (parse '{/ 6 3}) (AppC (IdC '/) (list (NumC 6) (NumC 3))))
(check-equal? (parse '{eq? 6 6}) (BinopC 'eq (NumC 6) (NumC 6)))
(check-equal? (parse '{<= 4 5}) (BinopC '<= (NumC 4) (NumC 5)))
(check-equal? (parse '3) (NumC 3))

; TODO: Why does this want a list of an AppC?
(check-equal? (parse '{g {+ 3 x}}) (AppC (IdC 'g) (list (AppC (IdC '+) (list (NumC 3) (IdC 'x))))))
(check-equal? (parse '{if {- 0 x} 0 1}) (ifC (AppC (IdC '-) (list (NumC 0) (IdC 'x))) (NumC 0) (NumC 1)))


; =======================================
;                Interp
; =======================================

;; Interps a list of args
; (: interp args (Listof ExprC -> 
(define (interp-args args env)
  (map (λ (x) (interp x env)) args))

;; Interpret an expression in a given environment
;(: interp (ExprC Env -> Value))
(define (interp expr env)
  (match expr
    [(NumC n) (NumV n)]
    [(StringC s) (StringV s)]
    [(LamC params body) (CloV params body env)]
    [(IdC i) (hash-ref env i (λ () (error "DFLY: no value found for key" i)))]
    [(ifC test then el)
     (define testResult (interp test env))
     (cond
       [(equal? testResult (BooleanV 'true)) (interp then env)]
       [else (interp el env)])]
    [(BinopC name l r)
     (define rVal (interp r env))
     (define lVal (interp l env))
     (match lVal
       [(NumV lVal)
        (match rVal
          [(NumV rVal)
           (cond
             [(equal? name 'eq)
              (cond
                [(eq? lVal rVal) (BooleanV 'true)]
                [else (BooleanV 'false)])]
             [(and (= 0 rVal) (equal? name 'divide)) (error "DFLY: Cannot divide by 0")]
             [(equal? name '<=)
              (cond
                [(<= lVal rVal) (BooleanV 'true)]
                [else (BooleanV 'false)])]
             
             [else (NumV ((hash-ref Operations name) lVal rVal))])]
          [other (error "DFLY: Cannot binop without a number")])]
        [other (error "DFLY: Cannot binop without a number")])]
    [(AppC fun args)
     (match (interp fun env)
       [(CloV params body clo-env)
        (cond
          ; If the num args doesn't match num params
          [(= (length params) (length args))
           (interp body (foldl  ; returns a new env containing the param/arg pairs
                         (λ (param arg new-env)
                           (hash-set new-env param arg))
                         clo-env
                         params
                         (interp-args args env)))]
          [else  (error "DFLY: wrong number of arguments given")])])]))


(check-equal? (interp (parse '{<= 3 4}) top-env) (BooleanV 'true))
(check-equal? (interp (parse '{<= 9 4}) top-env) (BooleanV 'false))
(check-equal? (interp (ifC (IdC 'false) (IdC 'true) (AppC (IdC '+) (list (NumC 1) (NumC 2)))) top-env) (NumV 3)) 
(check-equal? (interp (ifC (IdC 'true) (IdC 'false) (AppC (IdC '+) (list (NumC 1) (NumC 2)))) top-env) (BooleanV 'false))
(check-equal? (interp (parse "roar") top-env) (StringV "roar"))

(check-equal? (interp (parse '{eq? 3 4}) top-env) (BooleanV 'false))
(check-equal? (interp (parse '{eq? 5 5}) top-env) (BooleanV 'true))

(check-equal? (interp (AppC (IdC '+) (list (NumC 1) (NumC 2))) top-env) (NumV 3))

(check-equal? (interp (parse '{+ 3 4}) top-env) (NumV 7))
(check-equal? (interp (parse '{* 3 {+ 4 5}}) top-env) (NumV 27))

(check-equal? (interp (parse '{v {+ 3 4}})
                      (hash-union
                       (hash
                        'z (CloV (list 'q) (parse '{+ q 1}) top-env)
                        'v (CloV (list 'q) (parse '{+ q -1}) top-env))
                       top-env))
              (NumV 6))

(check-equal? (interp (parse '{lam {a b} {+ a b}}) top-env)
              (CloV (list 'a 'b) (AppC (IdC '+) (list (IdC 'a) (IdC 'b))) top-env))


(check-exn #px"DFLY: Cannot divide by 0"
           (λ () (interp (parse '{/ 1 0}) top-env)))

(check-exn #px"DFLY: no value found for key"
           (λ () (interp (parse '{calls-b 3})
                         (hash
                          'calls-b (CloV (list 'x) (parse '{b 24}) top-env)
                          'b       (CloV (list 'y) (parse '{+ x y}) top-env)))))

;; Turn a DFLY3 value to a string
; (: serialize (DFLY3 -> String))
(define (serialize x)
  (match x
    [(NumV n) (~v n)]
    [(NumC n) (~v n)]
    [(StringV s) s]
    [(CloV params body env) "#<procedure>"]
    [(BooleanV bool)
     (cond
       [(equal? bool 'true) "true"]
       [(equal? bool 'false) "false"])]))

(check-equal? (serialize (NumC 2)) "2")
(check-equal? (serialize (NumV 1)) "1")
(check-equal? (serialize (CloV (list 'x) (parse '{+ 1 2}) top-env)) "#<procedure>")
(check-equal? (serialize (BooleanV 'true)) "true")
(check-equal? (serialize (BooleanV 'false)) "false")
(check-equal? (serialize (StringV "poop")) "poop")

;; Combine parsing and evaluation
; (: top-interp (sexp -> string)
(define (top-interp s)
  (serialize (interp (parse s) top-env)))

;(check-equal? (top-interp '{lam {+ 2 3}}) "5")
(check-equal? (top-interp 'true) "true")

(check-exn #px"DFLY: wrong number of arguments given"
           (λ () (top-interp '((lam () 9) 17))))

(check-exn #px"DFLY: Cannot binop without a number"
           (λ () (top-interp '(+ + +))))


(check-exn #px"DFLY: Cannot binop without a number"
           (λ () (top-interp '(+ 3 +))))

;lam {a b} {b {+ 2 3}}

; Captain Teach:
(check-equal? (top-interp
               '{with {x = 9}
                      {+ x 1}}) "10")

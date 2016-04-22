#lang racket
(require rackunit)

; TODO
; [x] Binop datatype
; [.] Multiple function arguments
; [ ] Main
; [x] Conditional: ifleq0
; [ ] Finish EBNF
; Interface:
;    [x] parse
;    [.] parse-fundef
;    [x] parse-prog
;    [.] interp-fns
;    [x] interp
;    [x] top-interp

;(define-type ExprC (U NumC IdC BinopC AppC ifleq0C FundefC))
(struct NumC (n) #:transparent)
(struct IdC (x) #:transparent)
(struct AppC (fun arg) #:transparent) ; use of a function (call)
(struct ifleq0C (n then else) #:transparent)
(struct FundefC (name args body) #:transparent)
(struct BinopC (name left right) #:transparent)

;symbol -> actual operator ex. '+ +
(define Operations (hash
                    'plus +
                    'mult *
                    'minus -
                    'divide /))

(define funs (list
              (FundefC 'add-three (list (IdC 'x)) (BinopC 'plus (IdC 'x) (NumC 3)))
              (FundefC 'double-plus-one (list (IdC 'x)) (BinopC 'plus (BinopC 'mult (NumC 2) (IdC 'x)) (NumC 1)))
              (FundefC 'add-two-nums (list (IdC 'x) (IdC 'y)) (BinopC 'plus (IdC 'x) (IdC 'y)))
              (FundefC 'main (list) (NumC 7)) 
              ))

;; Interpret the plus, mult, and num ArithC's
;(: interp (ExprC (Listof FundefC) -> Real))
(define (interp exp funs)
  (match exp
    [(NumC n) n]
    [(BinopC name l r) ((hash-ref Operations name) (interp l funs) (interp r funs))]
    [(AppC f a) (local ([define fd (get-fundef f funs)])
                  (interp (foldl (λ (var val result)
                                   (subst var val result))
                                 (FundefC-body fd) ; initial value
                                 (FundefC-args fd) ; vars to be sub'd
                                 a)                ; vals to sub in
                          
                          funs))]
    [(ifleq0C test then el)
     (cond
       [(<= (interp test funs) 0) (interp then funs)]
       [else (interp el funs)])]))


(check-equal? (interp (NumC 6) funs) 6)
(check-equal? (interp (BinopC 'plus (NumC 2) (NumC 3)) funs) 5)
(check-equal? (interp (BinopC 'mult (NumC 4) (NumC 2)) funs) 8)
(check-equal? (interp (ifleq0C (BinopC 'minus (NumC 0) (NumC 1)) (NumC 0) (NumC 1)) funs) 0)
(check-equal? (interp (ifleq0C (BinopC 'minus (NumC 1) (NumC 0)) (NumC 0) (NumC 1)) funs) 1)

;; Look up a function from a list by name
;(: get-fundef (IdC (Listof FundefC) -> FundefC))
(define (get-fundef name funs)
  (match funs
    ['() (error 'fun-lookup "DFLY: function not found with name: ~e" name)]
    [(cons fst rst)
     (cond [(equal? (FundefC-name fst) name)
            fst]
           [else (get-fundef name rst)])]))


;; Interpret main
;(: interp-fns ((Listof FundefC) -> Real))
(define (interp-fns complete-funs funs)
  (cond
    [(string=? (symbol->string (FundefC-name (first funs))) "main")
     (interp (FundefC-body (first funs)) complete-funs)]
    [else (interp-fns complete-funs (rest funs))]))

;(check-equal? (interp-fns complete-funs funs) 7)

;; replace instances of 'from' with 'to' in 'in'
;(: subst (IdC ExprC ExprC -> ExprC))
(define (subst from to in)
  (match in
    [(IdC x) (cond [(equal? x from) to]
                   [else (IdC x)])]
    [(NumC n) (NumC n)]
    [(BinopC name l r) (BinopC name (subst from to l)
                                    (subst from to r))]
    [(ifleq0C test then els) (ifleq0C (subst from to test) (subst from to then) (subst from to els))]
    [(AppC fun args) (AppC fun (map (λ (arg)
                                      (subst from to arg))
                                    args))]))

; helper for multiple argument subs - interp needs to apply 4 to x, 9 to y, etc
; but subst needs to sub in 4 for x in as many places as x appears


;; Parser - Converts a Sexp to an ExprC
;(: parse (Sexp -> ExprC))
(define (parse sexp)
  (match sexp
    [(? real? n) (NumC n)]
    [(? symbol? x) (IdC x)]
    [(list 'ifleq0 test then el) (ifleq0C (parse test) (parse then) (parse el))]
    [(list '+ l r) (BinopC 'plus (parse l) (parse r))]
    [(list '- l r) (BinopC 'minus (parse l) (parse r))]
    [(list '* l r) (BinopC 'mult (parse l) (parse r))]
    [(list '/ l r) (BinopC 'divide (parse l) (parse r))]
    [(list (? symbol? fun) arg ...)
     (AppC fun (map parse arg))]
    [other (error "DFLY: Oh noes, input not well-formed")]))

(check-exn #px"DFLY: Oh noes, input not well-formed" (λ () (parse '(16))))
(check-equal? (parse '{+ 1 2}) (BinopC 'plus (NumC 1) (NumC 2)))
(check-equal? (parse '{* 2 3}) (BinopC 'mult (NumC 2) (NumC 3)))
(check-equal? (parse '{- 4 2}) (BinopC 'minus (NumC 4) (NumC 2)))
(check-equal? (parse '{/ 6 3}) (BinopC 'divide (NumC 6) (NumC 3)))
(check-equal? (parse '3) (NumC 3))
(check-equal? (parse '{g {+ 3 x}}) (AppC 'g (list (BinopC 'plus (NumC 3) (IdC 'x)))))
(check-equal? (parse '{ifleq0 {- 0 x} 0 1}) (ifleq0C (BinopC 'minus (NumC 0) (IdC 'x)) (NumC 0) (NumC 1)))

;; parse an s-expression into a FundefC
;(: parse-fundef (Sexp -> FundefC))
(define (parse-fundef s)
  (match s
    [(list 'func (? symbol? name) (list (? symbol? args) ...) body)
     (FundefC name args (parse body))]))

(check-equal? (parse-fundef '{func g {x} {* x 9}}) (FundefC 'g (list 'x) (BinopC 'mult (IdC 'x) (NumC 9))))

;(define fundefTest (FundefC 'addStuff 'a 'b {PlusC a b}))
;(check-equal? (parse-fundef '{func addStuff {a b} {+ a b}})
;              (FundefC 'addStuff (list 'a 'b) (BinopC 'plus (IdC 'a) (IdC 'b))))

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
;(struct FundefC ([name : Symbol] [args : Symbol] [body : ExprC]) #:transparent)



;; Evaluates an Sexp by calling parse and interp
;(: top-interp ((Listof Sexp) -> Real))
(define (top-interp fun-sexps)
  (interp-fns (parse-prog fun-sexps) (parse-prog fun-sexps)))

(check-equal? (top-interp
               '{{func add-two {x} {+ 2 x}}
                 {func main {} {add-two 4}}}) 6)
#lang typed/racket
(require typed/rackunit)

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

(define-type ExprC (U NumC IdC BinopC AppC ifleq0C FundefC))
(struct NumC ([n : Real]) #:transparent)
(struct IdC ([x : Symbol]) #:transparent)
(struct AppC ([fun : Symbol] [arg : ExprC]) #:transparent)
(struct ifleq0C ([n : ExprC] [then : ExprC] [else : ExprC]) #:transparent)
(struct FundefC ([name : Symbol] [args : (Listof Symbol)] [body : ExprC]) #:transparent)
(struct BinopC ([name : Symbol] [left : ExprC] [right : ExprC]) #:transparent)

;symbol -> actual operator ex. '+ +
(define Operations (hash
                    'plus +
                    'mult *
                    'minus -
                    'divide /))

(define funs (list))

;(check-equal? {ifleq0C -1 { #true} {#false} } #true)

;; Evaluates an Sexp by calling parse, desugar and interp
#;(
(: top-interp (Sexp -> Real))
(define (top-interp fun-sexps)
  (interp-fns (parse-prog fun-sexps)))

(check-equal? (top-interp '{+ 1 2}) 3)
(check-equal? (top-interp '{- 6 2}) 4)
(check-equal? (top-interp '{* 6 2}) 12)
)

;; Interpret the plus, mult, and num ArithC's
(: interp (ExprC (Listof FundefC) -> Real))
(define (interp [exp : ExprC] [funs : (Listof FundefC)]) : Real
  (match exp
    [(NumC n) n]
    [(BinopC name l r) ((hash-ref Operations name) (interp l funs) (interp r funs))] 
    [(ifleq0C test then el)
     (cond
       [(<= (interp test funs) 0) (interp then funs)]
       [else (interp el funs)])]))

(check-equal? (interp (NumC 6) funs) 6)
(check-equal? (interp (BinopC 'plus (NumC 2) (NumC 3)) funs) 5)
(check-equal? (interp (BinopC 'mult (NumC 4) (NumC 2)) funs) 8)

; Interpret main
(: interp-fns ((Listof FundefC) -> Real))
(define (interp-fns [funs : (Listof FundefC)]) : Real
  (cond
    [(string=? (symbol->string (FundefC-name (first funs))) "main") (interp (FundefC-body (first funs)) funs)]
    [else (interp-fns (rest funs))]))

; TODO test this
    

;; Parser - Converts a Sexp to an ExprC
(: parse (Sexp -> ExprC))
(define (parse [sexp : Sexp]) : ExprC
  (match sexp
    [(? real? n) (NumC n)]
    [(list '+ l r) (BinopC 'plus (parse l) (parse r))]
    [(list '- l r) (BinopC 'minus (parse l) (parse r))]
    [(list '* l r) (BinopC 'mult (parse l) (parse r))]
    [(list '/ l r) (BinopC 'divide (parse l) (parse r))]
    [(list 'ifleq0 test then el) (ifleq0C (parse test) (parse then) (parse el))]
    [other (error "DFLY: Oh noes, input not well-formed")]))

(check-exn #px"DFLY: Oh noes, input not well-formed" (λ () (parse '(+ 1))))
(check-equal? (parse '{+ 1 2}) (BinopC 'plus (NumC 1) (NumC 2)))
(check-equal? (parse '{* 2 3}) (BinopC 'mult (NumC 2) (NumC 3)))
(check-equal? (parse '{- 4 2}) (BinopC 'minus (NumC 4) (NumC 2)))


;; parse an s-expression into a FundefC
(: parse-fundef (Sexp -> FundefC))
(define (parse-fundef [s : Sexp]) : FundefC
  (match s
    ;[(list 'func (? symbol? name) (list (____) body) (FundefC name (list) body))
    [(list 'func (? symbol? name) (list (? symbol? args) ...) body)
     (FundefC name
              (map (λ (a) (cast a Symbol)) args) (parse body))]))


;(define fundefTest (FundefC 'addStuff 'a 'b {PlusC a b}))
(check-equal? (parse-fundef '{func addStuff {a b} {+ a b}})
              (FundefC 'addStuff (list 'a 'b) (BinopC 'plus (IdC 'a) (IdC 'b))))
;; Turns a list of s-expressions to a list of FundefC's
(: parse-prog ((Listof Sexp) -> (Listof FundefC)))
(define (parse-prog [s : (Listof Sexp)]) : (Listof FundefC)
   ((inst map FundefC Sexp) parse-fundef s))


;(check-equal? (parse-prog '{addStuff {a b} {+ a b}}) (fundefTest)) 
;(struct FundefC ([name : Symbol] [args : Symbol] [body : ExprC]) #:transparent)
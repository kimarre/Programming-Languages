#lang typed/racket
(require typed/rackunit)

(define-type ExprC (U NumC IdC PlusC MultC MinusC AppC ifleq0C))
(struct NumC ([n : Real]) #:transparent)
(struct IdC ([x : Symbol]) #:transparent)
(struct PlusC ([left : ExprC] [right : ExprC]) #:transparent)
(struct MultC ([left : ExprC] [right : ExprC]) #:transparent)
(struct MinusC ([left : ExprC] [right : ExprC]) #:transparent)
(struct DivideC ([left : ExprC] [right : ExprC]) #:transparent)
(struct AppC ([fun : Symbol] [arg : ExprC]) #:transparent)
(struct ifleq0C ([n : ExprC] [then : ExprC] [else : ExprC]) #:transparent)
(struct FundefC ([name : Symbol] [args : Symbol] [body : ExprC]) #:transparent)

(define funs (list))

;(check-equal? {ifleq0C -1 { #true} {#false} } #true)

; Evaluates an Sexp by calling parse, desugar and interp
#;((: top-interp (Sexp -> Real))
(define (top-interp fun-sexps)
  (interp-fns (parse-prog fun-sexps)))


(check-equal? (top-interp '{+ 1 2}) 3)
(check-equal? (top-interp '{- 6 2}) 4)
(check-equal? (top-interp '{* 6 2}) 12)


(check-equal? (top-interp
               '{{func odd {n} {
                                {{ifleq0 n
                                         {ifleq0 {+ n 1} ; +1 to differentiate -1 (odd) from 0 (even) in ifleq0
                                                 1
                                                 0}
                                         {odd {- n 2}}}}}}
                 {func even {n} {
                                 {ifleq0 n
                                         {ifleq0 {+ n 1}
                                                 0
                                                 1}
                                         {even {- n 2}}}}}
                 {func main {} {even 4}}}) 1)
)

; Interpret the plus, mult, and num ArithC's
(: interp (ExprC (Listof FundefC) -> Real))
(define (interp [exp : ExprC] [funs : (Listof FundefC)]) : Real
  (match exp
    [(NumC n) n]
    [(PlusC l r) (+ (interp l funs) (interp r funs))]
    [(MultC l r) (* (interp l funs) (interp r funs))]
    [(MinusC l r) (- (interp l funs) (interp r funs))]
    [(ifleq0C test then el)
     (cond
       [(<= (interp test funs) 0) (interp then funs)]
       [else (interp el funs)])]))

(check-equal? (interp (NumC 6) funs) 6)
(check-equal? (interp (PlusC (NumC 2) (NumC 3)) funs) 5)
(check-equal? (interp (MultC (NumC 4) (NumC 2)) funs) 8)

; Parser - Converts a Sexp to an ExprC
(: parse (Sexp -> ExprC))
(define (parse [sexp : Sexp]) : ExprC
  (match sexp
    [(? real? n) (NumC n)]
    [(list '+ l r) (PlusC (parse l) (parse r))]
    [(list '- l r) (MinusC (parse l) (parse r))]
    [(list '* l r) (MultC (parse l) (parse r))]
    [(list 'ifleq0 test then el) (ifleq0C (parse test) (parse then) (parse el))]
    [other (error "Oh noes, input not well-formed")]))

(check-exn #px"Oh noes, input not well-formed" (λ () (parse '(+ 1))))
(check-equal? (parse '{+ 1 2}) (PlusC (NumC 1) (NumC 2)))
(check-equal? (parse '{* 2 3}) (MultC (NumC 2) (NumC 3)))
(check-equal? (parse '{- 4 2}) (MinusC (NumC 4) (NumC 2)))

;; parse an s-expression into a Fundef
(: parse-fundef (Sexp -> FundefC))
(define (parse-fundef sexp)
  (match sexp
    [(list 'func (? symbol? name) (? symbol? param) body)
     (FundefC name param (parse body))]))

#;(
;(: parse-fundef (s -> FundefC)
(define (parse-fundef [s : Sexp]) : FundefC
  (match s
    [(func name {} body) (FundefC name (list) body)]
    [(func name {x ...} body) (FundefC name x body)]))
 )

;parse-prog
(define (parse-prog [s : Sexp]) : (Listof FundefC)
   (map parse s))
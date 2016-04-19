#lang typed/racket
(require typed/rackunit)

(define-type FundefC [fdC (name : symbol) (arg : symbol) (body : ExprC)])
(define-type ExprC (U NumC IdC PlusC MultC AppC ifleq0))
(struct numC ([n : Real]) #:transparent)
(struct IdC ([x : Symbol]) #:transparent)
(struct plusC ([left : ExprC] [right : ExprC]) #:transparent)
(struct multC ([left : ExprC] [right : ExprC]) #:transparent)
(struct appC ([fun : Symbol] [arg : ExprC]) #:transparent)
(struct ifleq0 ([n : NumC] [then : ExprC] [else : ExprC]) #:transparent)

; Evaluates an Sexp by calling parse, desugar and interp
(: top-interp (Sexp -> Number))
(define (top-interp fun-sexps)
  (interp-fns (parse-prog fun-sexps)))

(check-equal? (top-interp '{+ 1 2}) 3)
(check-equal? (top-interp '{- 6 2}) 4)
(check-equal? (top-interp '{* 6 2}) 12)

(check-equal? (top-interp
               '{{func odd {x} {
                                {{ifleq0 n
                                         {ifleq0 {+ n 1} ; +1 to differentiate -1 (odd) from 0 (even) in ifleq0
                                                 1
                                                 0}
                                         {odd {- n 2}}}}}}
                 {func even {x} {
                                 {ifleq0 n
                                         {ifleq0 {+ n 1}
                                                 0
                                                 1}
                                         {even {- n 2}}}}}}))

; Interpret the plus, mult, and num ArithC's
(: interp (ExprC (listof FundefC) -> Number))
(define (interp [exp : ExprC] [funs : (listof FundefC)]) : Number
  (match exp
    [(numC n) n]
    [(plusC l r) (+ (interp l) (interp r))]
    [(multC l r) (* (interp l) (interp r))]))

(check-equal? (interp (numC 6)) 6)
(check-equal? (interp (plusC (numC 2) (numC 3))) 5)
(check-equal? (interp (multC (numC 4) (numC 2))) 8)

; Parser - Converts a Sexp to an ExprC
(: parse (Sexp -> ExprC))
(define (parse [sexp : Sexp]) : ExprC
  (match sexp
    [(? number? n) (numC n)]
    [(list '+ l r) (plusC (parse l) (parse r))]
    ;[(list '- l r) (bminusC (parse l) (parse r))]
    ;[(list '- n) (uminusC (parse n))]
    [(list '* l r) (multC (parse l) (parse r))]
    [other (error "Oh noes, input not well-formed")]))

(check-exn #px"Oh noes, input not well-formed" (λ () (parse '(+ 1))))
(check-equal? (parse '{+ 1 2}) (plusC (numC 1) (numC 2)))
;(check-equal? (parse '{- 4}) (uminusS (parse 4)))
(check-equal? (parse '{* 2 3}) (multC (numC 2) (numC 3)))
;(check-equal? (parse '{- 5 2}) (bminusS (numS 5) (numS 2)))
;(check-equal? (parse '{- 3}) (uminusS (numC 3)))

#lang typed/racket
(require typed/rackunit)

(define-type ArithC (U numC plusC multC))
[struct numC ([n : Number]) #:transparent]
[struct plusC ([l : ArithC] [r : ArithC]) #:transparent]
[struct multC ([l : ArithC] [r : ArithC]) #:transparent]

; Interpret the plus, mult, and num ArithC's
(: interp [ArithC -> Number])
(define (interp [a : ArithC]) : Number
  (match a
    [(numC n) n]
    [(plusC l r) (+ (interp l) (interp r))]
    [(multC l r) (* (interp l) (interp r))]))

(check-equal? (interp(numC 6)) 6)
(check-equal? (interp(plusC (numC 2) (numC 3))) 5)
(check-equal? (interp(multC (numC 4) (numC 2))) 8)


; Returns a number indicating how many numbers it contains
(: num-nums [ArithC -> Number])
(define (num-nums [a : ArithC]) : Number
  (match a
    [(numC n) 1]
    [(plusC l r) 2]
    [(multC l r) 2]))

(check-equal? (num-nums (numC 5)) 1)
(check-equal? (num-nums (plusC (numC 2) (numC 1))) 2)
(check-equal? (num-nums (multC (numC 1) (numC 1))) 2)


(define-type ArithS (U numS plusS bminusS uminusS multS))
(struct numS ([n : Number]) #:transparent)
(struct plusS ([l : ArithS] [r : ArithS]) #:transparent)
(struct bminusS ([l : ArithS] (r : ArithS)) #:transparent) 
(struct uminusS ([n : ArithS]) #:transparent)
(struct multS ([l : ArithS] [r : ArithS]) #:transparent)


; Parser - Converts a Sexp to an ArithS
(: parse (Sexp -> ArithS))
(define (parse [sexp : Sexp]) : ArithS
  (match sexp
    [(? number? n) (numS n)]
    [(list '+ l r) (plusS (parse l) (parse r))]
    [(list '- l r) (bminusS (parse l) (parse r))]
    [(list '- n) (uminusS (parse n))]
    [(list '* l r) (multS (parse l) (parse r))]
    [other (error "Oh noes, input not well-formed")]))

(check-exn #px"Oh noes, input not well-formed" (λ () (parse '(+ 1))))
(check-equal? (parse '{+ 1 2}) (plusS (numS 1) (numS 2)))
(check-equal? (parse '{- 4}) (uminusS (parse 4)))
(check-equal? (parse '{* 2 3}) (multS (numS 2) (numS 3)))
(check-equal? (parse '{- 5 2}) (bminusS (numS 5) (numS 2)))
(check-equal? (parse '{- 3}) (uminusS (numS 3)))

; Desugar - Converts an ArithS to an ArithC, num plus mult
(: desugar [ArithS -> ArithC])
(define (desugar [s : ArithS]) : ArithC
  (match s
    [(numS n) (numC n)]
    [(plusS l r) plusC (desugar l) (desugar r)]
    [(bminusS l r) (plusC (desugar l) (multC (numC -1) (desugar r)))] ; l - r = l + -1 * r
    [(uminusS n) (multC (numC -1) (desugar n))]
    [(multS l r) (multC (desugar l) (desugar r))]
    [other (error "Oh noes, bad input for ArithS")]))

(check-equal? (desugar (numS 5)) (numC 5))


  ; CMD + \ makes λ



















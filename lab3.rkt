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
    [(plusS l r) (plusC (desugar l) (desugar r))]
    [(bminusS l r) (plusC (desugar l) (multC (numC -1) (desugar r)))] ; l - r = l + -1 * r
    [(uminusS n) (multC (numC -1) (desugar n))]
    [(multS l r) (multC (desugar l) (desugar r))]))

(check-equal? (desugar (numS 5)) (numC 5))
(check-equal? (desugar (plusS (numS 3) (numS 4))) (plusC (numC 3) (numC 4)))
(check-equal? (desugar (bminusS (numS 6) (numS 3))) (plusC (numC 6) (multC (numC -1) (numC 3))))
(check-equal? (desugar (uminusS (numS 4))) (multC (numC -1) (numC 4)))
(check-equal? (desugar (multS (numS 4) (numS 4))) (multC (numC 4) (numC 4)))
 
; Evaluates an Sexp by calling parse, desugar and interp
(: top-interp [Sexp -> Number])
(define (top-interp [s : Sexp])
  (interp (desugar (parse s))))

(check-equal? (top-interp '{+ 1 2}) 3)
(check-equal? (top-interp '{- 6 2}) 4)
(check-equal? (top-interp '{* 6 2}) 12)

; Applies the given function to the list twice
(: doublemap ((Number -> Number) (Listof Number) -> (Listof Number)))
(define (doublemap [f : (Number -> Number)] [l : (Listof Number)]) : (Listof Number)
  (map (λ (number)
      (f number))
       (map (λ (number)
              (f number))
            l))
  )

; Example number manipulation function for doublemap
(: triple-it (Number -> Number))
(define (triple-it [num : Number]) : Number
  (* 3 num))

(check-equal? (doublemap triple-it (list 1 2 3)) (list 9 18 27))

; Returns a list of lists where each element is a list of corresponding elements
; from the original lists
(: zip [(Listof Number) (Listof Number) -> (Listof (Listof Number))])
(define (zip [l1 : (Listof Number)] [l2 : (Listof Number)])
  (cond
    [(empty? l1) empty]
    [else (append (list (list (first l1) (first l2))) (zip (rest l1) (rest l2)))]))

(check-equal? (zip (list 1 2 3) (list 4 5 6)) (list (list 1 4) (list 2 5) (list 3 6)))







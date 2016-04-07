#lang racket
(require rackunit)

; #### Problem 2.3.3 ####
; Calculates the total income the attendees of a theater produce
; (: total-profit [Number -> Number])
(define (total-profit numPeople)
  (- (* 5 numPeople) (+ 20 (* numPeople .5))))

(check-equal? (total-profit 5) 2.5)
(check-equal? (total-profit 2) -11.0)
(check-equal? (total-profit 20) 70.0)

; #### Problem 3.3.3 ####
; Calculates the total surface area
; (: area-cylinder (Number Number -> Number))
(define (area-cylinder radius height)
  (+ (* (* (* 2 pi) radius) height) (* (* (* radius radius) pi) 2)))

(check-= (area-cylinder 2 5) 87.96 .01)


; #### Magic Tricks ####

; represent a magic trick
; (define-type Trick (U Card-Trick Guillotine))
(struct Card-Trick (decks volunteers) #:transparent)
(struct Guillotine (realism has-blood?) #:transparent)

; Determines how long a trick will take
; (: trick-minutes [Trick -> Number])
(define (trick-minutes trick)
  (match trick
    [(Card-Trick d v) (* 2 v d)]
    [(Guillotine r b)
       (cond
         [(equal? b #true) 20]
         [else 10])]))

(check-equal? (trick-minutes (Card-Trick 1 2)) 4)
(check-equal? (trick-minutes (Guillotine #false #false)) 10)
(check-equal? (trick-minutes (Guillotine #true #true)) 20)


; #### Low-degree Polynomials ####

; ((define-type) Polynomial (U linear quadratic))
; where a, b, and c are the coefficients
(struct linear (a b) #:transparent)       ; a may be zero
(struct quadratic (a b c) #:transparent)  ; a may not be zero

; Produces the result of plugging in the value x for the equation
; (: interp [Polynomial Number -> Number])
(define (interp polynomial x)
  (match polynomial
    [(struct linear (a b)) (+ (* a x) b)]
    [(struct quadratic (a b c)) (+ (* a x x) (* b x) c)]))

(check-equal? (interp (linear 2 3) 2) 7)
(check-equal? (interp (linear 0 1) 2) 1)
(check-equal? (interp (quadratic 2 3 4) 2) 18)


; #### Derivative ####
; Given a polynomial, returns another polynomial representing the derivative
; (: derivative [polynomial] -> polynomial)
(define (derivative polynomial)
  (match polynomial
    [(struct linear (a b)) (linear 0 a)]
    [(struct quadratic (a b c)) (linear (* 2 a) b)]))

(check-equal? (derivative (linear 2 3)) (linear 0 2))
(check-equal? (derivative (quadratic 2 3 4)) (linear 4 3))
  

; #### Binary Tree ####
; (define-type BTree(U Leaf Node))
(struct Leaf(symbol) #:transparent)
(struct Node(left right) #:transparent)

; examples of data
(define l1 (Leaf 'green))
(define l2 (Leaf 'corgi))
(define l3 (Leaf 'grumpycat))

(define n1 (Node l1 l2))
(define n2 (Node n1 l3))


; #### Min-Depth ####
; determines the length of the shortest path to a leaf
; (: min-depth [tree -> Number])
(define (min-depth tree)
  (traverse tree 1))

(define (traverse tree num)
  (match tree
      [(struct Node(l r))
        (traverse l (+ 1 num))
        (traverse r (+ 1 num))]
      [(struct Leaf(s)) num]))
  
(check-equal? (min-depth n2) 2)






















         
#lang racket
(require rackunit)

; Kim Arre
; CPE 430 - Clements
; Spring 2016

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
    [(Card-Trick d v) (* d (expt 2 v))]
    [(Guillotine r b)
       (cond
         [(equal? b #true) 20]
         [else 10])]))

(check-equal? (trick-minutes (Card-Trick 1 2)) 4)
(check-equal? (trick-minutes (Card-Trick 3 4)) 48)
(check-equal? (trick-minutes (Guillotine #false #false)) 10)
(check-equal? (trick-minutes (Guillotine #true #true)) 20)


; #### Low-degree Polynomials ####

; ((define-type) Polynomial (U Linear Quadratic))
; where a, b, and c are the coefficients
(struct Linear (a b) #:transparent)       ; a may be zero
(struct Quadratic (a b c) #:transparent)  ; a may not be zero

; Produces the result of plugging in the value x for the equation
; (: interp [Polynomial Number -> Number])
(define (interp polynomial x)
  (match polynomial
    [(struct Linear (a b)) (+ (* a x) b)]
    [(struct Quadratic (a b c)) (+ (* a x x) (* b x) c)]))

(check-equal? (interp (Linear 2 3) 2) 7)
(check-equal? (interp (Linear 0 1) 2) 1)
(check-equal? (interp (Quadratic 2 3 4) 2) 18)


; #### Derivative ####
; Given a polynomial, returns another polynomial representing the derivative
; (: derivative [polynomial] -> polynomial)
(define (derivative polynomial)
  (match polynomial
    [(struct Linear (a b)) (Linear 0 a)]
    [(struct Quadratic (a b c)) (Linear (* 2 a) b)]))

(check-equal? (derivative (Linear 2 3)) (Linear 0 2))
(check-equal? (derivative (Quadratic 2 3 4)) (Linear 4 3))
  

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
  (traverse tree 0))

(define (traverse tree num)
  (match tree
      [(struct Node(l r))
        (min (traverse l (+ 1 num)) (traverse r (+ 1 num)))]
      [(struct Leaf(s)) num]))
  
(check-equal? (min-depth n2) 1)


; #### Traversal ####
; Produces a list representing an in-order traversal of symbols at the leaves of a binary tree
; (: in-order [tree -> list])
(define (in-order tree)
  (match tree
    [(struct Node(l r))
       (append (in-order l) (in-order r))]
    [(struct Leaf(s)) (list s)]
    [else (list)]))

       

; Tests for in-order
(define H (Leaf 'H))
(define I (Leaf 'I))
(define E (Leaf 'E))
(define F (Leaf 'F))
(define J (Leaf 'J))

(define D (Node H I))
(define B (Node D E))
(define G (Node J null))
(define C (Node F G))
(define A (Node B C))
 
(check-equal? (in-order A) (list 'H 'I 'E 'F 'J))
(check-equal? (in-order B) (list 'H 'I 'E))
      
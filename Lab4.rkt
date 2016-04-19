#lang racket
(require rackunit)

((λ (x) (+ x 2)) 3)

((λ (f g) (f (g 3)))
 (λ (x) (+ x 3))     ; f
 (λ (x) (* x 2)))    ; g


; Takes 'a', returns a function that takes 'b' and adds the two together
;(: curried-add (number -> (number -> number))) 
(define (curried-add a)
  (λ (b) (+ a b)))

(check-equal? ((curried-add 2) 1) 3)


; Does crazy nesting of two functions
; (: (a b -> c) --> (a (b -> c)))
(define (curry2 func)
  (λ (a)
    (λ (b)
      (func a b))))

; Adds two numbers
; (: (number number -> number))
(define (addTwo x y)
  (+ x y))

(check-equal? (((curry2 addTwo) 3) 4) 7)


; Does crazy nesting of three functions
(define (curry3 func)
  (λ (a)
    (λ (b)
      (λ (c)
        (func a b c)))))


; Adds three numbers
; (: ((a b c -> d) --> (a -> (b -> (c -> d))))
(define (add3 a b c)
  (+ a b c))

(check-equal? ((((curry3 add3) 1) 2) 3) 6)

; returns true exactly when the symbol occurs in the list
(define (contains? list symbol)
  (cond
    [(empty? list) #false]
    [(symbol=? (first list) symbol) #true]
    [else (contains? (rest list) symbol)]))

(define list1 (list 'Apple 'Banana 'Cherry))
(check-equal? (contains? list1 'Cherry) #true)
(check-equal? (contains? list1 'Poop) #false)


; determines if corresponding symbol elements in two lists
; (: [listof symbols] [listof symbols] -> listof boolean)
(define (in-list-many? symbolList queryList)
  (map ((curry2 contains?) symbolList) queryList))

(define list2 (list 'Cat 'Banana 'Dog))
(define list3 (list 'monocle 'tophat 'mustache))

(check-equal? (in-list-many? list1 list2) (list #false #true #false))
(check-equal? (in-list-many? list1 list3) (list #false #false #false))

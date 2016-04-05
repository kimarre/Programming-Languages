#lang racket
(require htdp/image)
(require rackunit)

(+ 3 4)
(check-equal? (* 4 13) 52)

#true
#false
(or #false #true)
(check-equal? (or false true) true)

'a
'b
'thisisasymbol

; add two numbers together
(define (add-nums a b)
  (+ a b))

(add-nums 2 3)


; Exercise 14 - computes the volumne of a cube given the length of a side
(define (cube-volume length)
  (* length (* length length)))

(check-equal? (cube-volume 3) 27)


; Exercise 15 - extracts the first 1String
(define (string-first string)
  (substring string 0 1))

(check-equal? (string-first "herpderp") "h")


; Exercise 27 - Fix image classify
; takes an image and produces 'tall', 'wide' or 'square' for the larger dimension
(define (image-classify img)
  (cond
    [(> (image-height img) (image-width img)) "tall"]
    [(= (image-height img) (image-width img)) "square"]
    [(< (image-height img) (image-width img)) "wide"]))

(check-equal? (image-classify (rectangle 20 40 "outline" "black")) "tall")
(check-equal? (image-classify (rectangle 40 20 "outline" "black")) "wide")
(check-equal?(image-classify (rectangle 5 5 "outline" "black")) "square")
(check-equal? (image-classify (circle 3 "solid" "red")) "square")

; ===================================================================
; 4.2 Intervals
; Interest              
(define (interest deposit)
  (cond
    [(>= deposit 5000) (* deposit 1.05)]
    [(<= deposit 1000) (* deposit 1.04)]
    [(and (< deposit 5000) (> deposit 1000)) (* deposit 1.045)]))
  
(check-equal? (interest 1000) 1040.0)
(check-equal? (interest 5000) 5250.0)
(check-equal? (interest 4000) 4180.0)

; ====================================================================
; Structures
(struct Furniture (U desk))
(struct Desk (width height depth) #:transparent)
(struct Bookshelf (depth numShelves shelfWidth) #:transparent)
(Desk 4 3 2)

(define (furniture-footprint item)
  (match item
    [(struct Desk (w h d)) (* w d)]
    [(struct Bookshelf (d s w)) (* d w)]))

(check-equal? (furniture-footprint (Desk 4 3 2)) 8)
(check-equal? (furniture-footprint (Bookshelf 2 5 3)) 6)



  
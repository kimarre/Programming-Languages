#lang htdp/bsl
(require htdp/image)

(+ 3 4)
(check-expect (* 4 13) 52)

;#true
;#false
;(or #false #true)
;(check-expect (or false true) true)

'a
'b
'thisisasymbol

; add two numbers together
(define (add-nums a b)
  (+ a b))

(add-nums 2 3)


; Exercise 14 - computes the volumne of a cube given the length of a side
(check-expect (cube-volume 3) 27)

(define (cube-volume length)
  (* length (* length length)))


; Exercise 15 - extracts the first 1String
(check-expect (string-first "herpderp") "h")

(define (string-first string)
  (substring string 0 1))


; Exercise 27 - Fix image classify
; takes an image and produces 'tall', 'wide' or 'square' for the larger dimension
(check-expect(image-classify (rectangle 40 20 "outline" "black")) "wide")
(check-expect(image-classify (rectangle 5 5 "outline" "black")) "square")
(check-expect (image-classify (circle 3 "solid" "red")) "square")


;------------ TODO FINISH -------------
(define (image-classify img)
  (cond
    [(>= (image-height img) (image-width img)) "tall"]
    [(= (image-height img) (image-width img)) "square"]
    [(<= (image-height img) (image-width img)) "wide"]))


; ===================================================================
; 4.2 Intervals
; Interest
(check-expect (interest 1000) 1040)
(check-expect (interest 5000) 5250)
(check-expect (interest 4000) 4180)
                       
(define (interest deposit)
  (cond
    [(>= deposit 5000) (* deposit 1.05)]
    [(<= deposit 1000) (* deposit 1.04)]
    [(and (< deposit 5000) (> deposit 1000)) (* deposit 1.045)]))
  




  
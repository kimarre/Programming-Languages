#lang racket
(require rackunit)

; === 1 ===
; Outputs the string list as a single string concatenated in reverse order
; (: rev-str-app [list -> String])
(define (rev-str-app list)
   (foldr
      (lambda (current result)
         (string-append result current))
    ""
    list))

(check-equal? (rev-str-app (cons "apple" (cons "banana" (cons "cherry" empty)))) "cherrybananaapple")


; === 2 ===
; (define-type Processor (U Intel AMD ARM))
; where chip is a number
(struct Intel (chip)#:transparent)
(struct AMD (chip)#:transparent)
(struct ARM (chip)#:transparent) 



; === 3, 4, 5 ===
; Takes a list of processors and returns a list of only Intels
; (: onlyIntels [list -> list])
(define (onlyIntels list)
  (my-filter Intel? list))

; Takes a list of processors and returns a list of only AMDs
; (: onlyAMDs [list -> list])
(define (onlyAMDs list)
  (my-filter AMD? list))

; Returns a list of the type of processor in a list, specified by f
; (: onlyThese [list procedure -> list]
(define (onlyThese list f)
  (my-filter f list))

; Takes a filter checker procedure and builds a list of only that element
; (: my-filter [procedure list -> list])
(define (my-filter func list)
  (cond
    [(empty? list) empty]
    [(func (first list)) ; If the first matches the type we're looking for
      ; Start a new list with the element, and recurse down the rest
      (cons (first list) (my-filter func (rest list)))]
    [else (my-filter func (rest list))]))

(define procList (list (Intel 1) (Intel 2) (AMD 3) (ARM 4)))
(define emptyList (list empty))

; Processor Tests
(check-equal? (onlyIntels procList) (list (Intel 1) (Intel 2)))
(check-equal? (onlyIntels emptyList) empty)
(check-equal? (onlyAMDs procList) (list (AMD 3)))
(check-equal? (onlyThese procList ARM?) (list (ARM 4)))


; === 6 ===
; Appends the second list to the end of the first
; (: my-append [list list -> list]
(define (my-append list1 list2)
  (cond
    ; There's still stuff in list1, do that first
    [(not(empty? list1)) (cons (first list1) (my-append (rest list1) list2))]
    [(not(empty? list2)) (cons (first list2) (my-append list1 (rest list2)))]
    [else empty]))

(check-equal? (my-append (list 1 2 3) (list 4 5 6)) (list 1 2 3 4 5 6))

; === 7 ===
; Gives the list that remains after the first n elements
; (: my-drop [list Number -> list])
(define (my-drop list num)
  (cond
      [(> num 0) (my-drop (rest list) (- num 1))]
      [else list]))

(define testList (list "I" "am" "very" "sleepy" "now"))
(check-equal? (my-drop testList 2) (list "very" "sleepy" "now"))
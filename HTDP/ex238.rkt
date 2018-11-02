;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname ex238) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; String Los -> Boolean
; determines whether l contains the string s
(define (contains? s l)
  (cond
    [(empty? l) #false]
    [else (or (string=? (first l) s)
              (contains? s (rest l)))]))


; Los -> Boolean
; does l contain "atom"
(define (contains-atom? l)
  (contains? "atom" l))

; Los -> Boolean
; does l contain "basic"
(define (contains-basic? l)
  (contains? "basic" l))

; Los -> Boolean
; does l contain "zoo"
(define (contains-zoo? l)
  (contains? "zoo" l))

(define list1 (list -1 0 1 2))
(define list2 (list 0 1 2 3))
(define list3 (list 4 5 6 7))
(define list4 (list -3 -2 -1 0))

(check-expect (add1* list1) list2)
; Lon -> Lon
; adds 1 to each item on l
(define (add1* l)
  (addn 1 l))
     
(check-expect (plus5 list1) list3)
; Lon -> Lon
; adds 5 to each item on l
(define (plus5 l)
  (addn 5 l))

(check-expect (minus2 list1) list4)
(define (minus2 l)
  (addn -2 l))

; Number Lon -> Lon
; Adds Number to each member of a list-of-numbers
(check-expect (addn 1 list1) list2)
(check-expect (addn 5 list1) list3)
(define (addn n l)
  (cond
    [(empty? l) '()]
    [else (cons (+ n (first l)) (addn n (rest l)))]))

; Number Number -> Boolean
; is the area of a square with side x larger than c
(define (squared>? x c)
  (> (* x x) c))

(squared>? 3 10)
(squared>? 4 10)
(squared>? 5 10)

; Nelon -> Number
; determines the smallest 
; number on l
(define (inf l)
  (cond
    [(empty? (rest l))
     (first l)]
    [else
     (if (< (first l)
            (inf (rest l)))
         (first l)
         (inf (rest l)))]))
    

; Nelon -> Number
; determines the largest 
; number on l
(define (sup l)
  (cond
    [(empty? (rest l))
     (first l)]
    [else
     (if (> (first l)
            (sup (rest l)))
         (first l)
         (sup (rest l)))]))

; Operator Nelon -> Number
; produces the most extreme number in a list
(define (extreme o l)
  (cond
    [(empty? (rest l))
     (first l)]
    [else
     (if (o (first l)
            (extreme o (rest l)))
         (first l)
         (extreme o (rest l)))]))

(define list5 (list 25 24 23 22 21 20 19 18 17 16 15 14 13
      12 11 10 9 8 7 6 5 4 3 2 1))
 
(define list6 (list 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16
      17 18 19 20 21 22 23 24 25))

(check-expect (inf-1 list5) 1)
(check-expect (inf-1 list6) 1)
(define (inf-1 l)
  (extreme < l))

(check-expect (sup-1 list5) 25)
(check-expect (sup-1 list6) 25)
(define (sup-1 l)
  (extreme > l))
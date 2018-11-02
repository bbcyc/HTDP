;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex291) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))


; [X Y] [X Y -> Y] Y [List-of X] -> Y
; applies f from right to left to each item in lx and b
; (foldr f b (list x-1 ... x-n)) == (f x-1 ... (f x-n b))
;(define (foldr f b lx) ...)
 
;(foldr + 0 '(1 2 3 4 5))
;== (+ 1 (+ 2 (+ 3 (+ 4 (+ 5 0)))))
;== (+ 1 (+ 2 (+ 3 (+ 4 5))))
;== (+ 1 (+ 2 (+ 3 9)))
;== (+ 1 (+ 2 12))
;== (+ 1 14)


; [X Y] [X -> Y] [List-of X] -> [List-of Y]
; constructs a list by applying f to each item on lx
; (map f (list x-1 ... x-n)) == (list (f x-1) ... (f x-n))
;(define (map f lx) ...)

; [X Y] [X -> Y] [List-of X] -> [List-of Y]
(define (map-via-fold f l)
  

         
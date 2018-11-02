;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname ex272) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)

; [X Y] [X Y -> Y] Y [List-of X] -> Y
; applies f from right to left to each item in lx and b
; (foldr f b (list x-1 ... x-n)) == (f x-1 ... (f x-n b))
; (define (foldr f b lx) ...)

; List List -> List
; Appends one list to another using foldr
(check-expect (append-from-fold (list 1 2 3) (list 4 5 6 7 8))
               (list 1 2 3 4 5 6 7 8))
(define (append-from-fold list1 list2)
  (foldr cons list2 list1))

(check-expect (list-sum (list 1 2 3 4)) 10)
(define (list-sum lon)
  (foldr + 0 lon))

(check-expect (list-product (list 1 2 3 4)) 24)
(define (list-product lon)
  (foldl * 1 lon))

; List-of-Images -> Image
(
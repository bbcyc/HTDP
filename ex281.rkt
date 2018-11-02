;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex281) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)

; Number -> Boolean
(lambda (n) (< n 10))

; Number Number -> String
; returns product of numbers as string
(lambda (x y) (number->string (* x y)))

; Number -> Number
; returns 0 if even, 1 if odd
(lambda (n) (modulo n 2))

(define-struct ir [name price])
(lambda (ir1 ir2) (> (ir-price ir1) (ir-price ir2)))

(lambda (p i) (place-image
               (circle 5 "solid" "red")
               (posn-x p)
               (posn-y p)
               i))
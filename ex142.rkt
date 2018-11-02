;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ex142) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)

; ImageOrFalse is one of:
; – Image
; – #false

; List-of-images Number -> ImageOrFalse
; given a list-of-images loi and a number n
; returns first image that is not n x n, or
; #false if none found
(check-expect (ill-sized? '() 10) #false)
(check-expect (ill-sized? (cons (rectangle 10 10 "solid" "red") '()) 10)
              (rectangle 10 10 "solid" "red"))
(check-expect (ill-sized? (cons (rectangle 10 10 "solid" "red") '()) 5) #false)
(check-expect (ill-sized? (cons (rectangle 20 5 "solid" "blue")
                                (cons (rectangle 10 10 "solid" "red") '())) 10)
              (rectangle 10 10 "solid" "red"))

(define (ill-sized? loi n)
  (cond
    [(empty? loi) #false]
    [else (cond
            [(= (image-height (first loi)) (image-width (first loi)) n) (first loi)]
            [else (ill-sized? (rest loi) n)])]))
  
;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ex167) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; List-of-Posns -> Number
; returns sum of x-coordinates of a
; list of posns
(check-expect (sum (cons (make-posn 1 2) (cons (make-posn 3 4) (cons (make-posn -5 6) '())))) -1)
(define (sum lop)
  (cond
    [(empty? lop) 0]
    [else (+ (posn-x (first lop)) (sum (rest lop)))]))
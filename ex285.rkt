;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex285) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; Lon -> Lon
; converts list of US amounts to Euros
(check-expect (convert-euro (list 1 2 3)) (list 1.06 2.12 3.18))
(define (convert-euro lon)
  (map (lambda (n) (* 1.06 n)) lon))

; Lon -> Lon
; converts a list of fahr temps to cel
(check-expect (convertFC (list 32 212)) (list 0 100))
(define (convertFC lon)
  (map (lambda (n) (/ (* (- n 32) 5) 9)) lon))

; List-of-Posns -> List-of-lists-of-numbers
; Converts a list of posns into a
; list of lists of numbers
(check-expect (translate (list (make-posn 1 2) (make-posn 3 4)))
              (list (list 1 2) (list 3 4)))
(define (translate lop)
  (map (lambda (p) (list (posn-x p) (posn-y p))) lop))


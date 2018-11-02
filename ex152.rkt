;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ex152) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)

(define dot (circle 3 "solid" "red"))


; NaturalNumber Image -> Image
; produces an image or n copies of img
; in a vertical arrangement
(check-expect (col 3 dot)
 (place-images
  (list dot dot dot)
  (list (make-posn 10 10) (make-posn 10 20) (make-posn 10 30))
  (empty-scene 100 100)))
(define (col n img)
  {cond
    [(zero? n) (empty-scene 100 100)]
    [else (place-image img 10 (* n 10) (col (sub1 n) img))]})

; NaturalNumber Image -> Image
; produces an image or n copies of img
; in a vertical arrangement
(check-expect (row 3 dot)
 (place-images
  (list dot dot dot)
  (list (make-posn 10 10) (make-posn 20 10) (make-posn 30 10))
  (empty-scene 100 100)))
(define (row n img)
  (cond
    [(zero? n) (empty-scene 100 100)]
    [else (place-image img (* n 10) 10 (row (sub1 n) img))]))


(col 5 dot)
(row 5 dot)
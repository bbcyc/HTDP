;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ex111) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define-struct vec [x y])
; A vec is
;   (make-vec PositiveNumber PositiveNumber)
; interpretation represents a velocity vector

(define (checked-make-vec x y)
  (make-vec
    (cond
      [(and (number? x) (>= x 0)) x]
      [else (error "the first argument must be a positive number")])
    (cond
      [(and (number? y) (>= y 0)) y]
      [else (error "the second argument must be a positive number")])))
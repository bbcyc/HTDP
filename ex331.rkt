;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex331) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; A Dir.v1 (short for directory) is one of: 
; – '()
; – (cons File.v1 Dir.v1)
; – (cons Dir.v1 Dir.v1)
 
; A File.v1 is a String

(define tree.v1 (cons (cons (cons "part1" (cons "part2" (cons "part3" '()))) '())
                (cons "read!"
                (cons (cons (cons (cons "hang" (cons "draw" '())) '())
                      (cons (cons (cons "read!" '()) '()) '())) '()))))

; Dir.v1 -> Number
; Returns number of files
; in a given Dir.v1
(check-expect (how-many tree.v1) 7)
(define (how-many dir)
  (cond
    [(empty? dir) 0]
    [(string? dir) 1]
    [else
     (+ (how-many (first dir)) (how-many (rest dir)))]))
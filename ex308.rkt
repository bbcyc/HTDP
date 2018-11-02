;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex308) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/abstraction)

(define-struct phone [area switch four])

(define num1 (make-phone 713 111 2222))
(define num2 (make-phone 123 456 7890))
(define num3 (make-phone 713 333 4444))
(define num4 (make-phone 281 111 2222))
(define num5 (make-phone 281 333 4444))
(define input (list num1 num2 num3))
(define expect (list num4 num2 num5))

; List-of-Phone-Records -> LoPR
; Replaces every instance of area code
; 713 with 281 in a list of phone records
(check-expect (replace input) expect)
(define (replace lop)
  (for/list ([p lop])
    (match p
      [(phone 713 x y) (make-phone 281 x y)]
      [x x])))
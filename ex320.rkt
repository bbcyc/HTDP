;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex320) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; An S-expr is one of: 
; – Number
; – String
; – Symbol
; - '()
; – (cons S-expr [List-of S-expr])

          
(check-expect (count 'world 'hello) 0)
(check-expect (count '(world hello) 'hello) 1)
(check-expect (count '(((world) 3 "a" hello) hello) 'hello) 2)
(define (count sexp sy)
    (cond
      [(empty? sexp) 0]
      [(number? sexp) 0]
      [(string? sexp) 0]
      [(symbol? sexp) (if (symbol=? sexp sy) 1 0)]
      [else (+ (count (first sexp) sy) (count (rest sexp) sy))]))
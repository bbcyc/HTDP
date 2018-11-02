;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ex162) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define RATE 14)

; Number -> Number
; computes the wage for h hours of work
(define (wage h)
  (* RATE h))

; List-of-numbers -> List-of-numbers
; computes the weekly wages for the weekly hours
(check-expect (wage* '()) '())
(check-expect (wage* (cons 28 '())) (cons (* RATE 28) '()))
(check-expect (wage* (cons 4 (cons 2 '()))) (cons (* RATE 4) (cons (* RATE 2 ) '())))
(check-error (wage* (cons 4 (cons 2 (cons 120 '())))) "Wage hours too large, check again")

(define (wage* whrs)
  (cond
    [(empty? whrs) '()]
    [(> (first whrs) 100) (error "Wage hours too large, check again")] 
    [else (cons (wage (first whrs)) (wage* (rest whrs)))]))


;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ex163) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; List-of-Temperatures is one of 
; - (cons Number '())
; - (cons Number List-of-Temperatures)

(define F1 (cons 32 (cons 212 '())))
(define F2 (cons -202 (cons -4 (cons 104 '()))))

(define (convertOneFC f)
  (/ (* 5 (- f 32)) 9))

; LOTF -> LOTC
; converts a list of fahrenheit temps to a list of celsius temps
(check-expect (convertFC F1) (cons 0 (cons 100 '())))
(check-expect (convertFC F2) (cons -130 (cons -20 (cons 40 '()))))
(define (convertFC l)
  (cond
    [(empty? l) '()]
    [else (cons (convertOneFC (first l)) (convertFC (rest l)))]))
;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ex147) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; An NEList-of-Booleans is one of: 
; – (cons Boolean '())
; – (cons Boolean NEList-of-temperatures)
; interpretation non-empty lists of Booleans

; NEList-of-Booleans -> Boolean
; computes whether values in a list are #true 
(check-expect (all-true (cons #true (cons #true
  (cons #true '())))) #true)
(check-expect (all-true (cons #false (cons #true
  (cons #true '())))) #false)
(check-expect (all-true (cons #true (cons #true
  (cons #false '())))) #false)
(define (all-true ne-l)
  (cond
    [(empty? (rest ne-l)) (first ne-l)]
    [(false? (first ne-l)) #false]
    [else (all-true (rest ne-l))]))
 
; NEList-of-Booleans -> Boolean
; computes whether any values in a list are #true 
(check-expect (one-true (cons #true (cons #true
  (cons #true '())))) #true)
(check-expect (one-true (cons #false (cons #false
  (cons #true '())))) #true)
(check-expect (one-true (cons #false (cons #false
  (cons #false '())))) #false)
(define (one-true ne-l)
  (cond
    [(empty? (rest ne-l)) (first ne-l)]
    [(first ne-l) #true]
    [else (one-true (rest ne-l))]))
 
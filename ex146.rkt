;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ex146) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; An NEList-of-temperatures is one of: 
; – (cons CTemperature '())
; – (cons CTemperature NEList-of-temperatures)
; interpretation non-empty lists of Celsius temperatures

; NEList-of-temperatures -> Number
; computes the average temperature 
 
(check-expect (average (cons 1 (cons 2 (cons 3 '()))))
              2)
 
(define (average ne-l)
  (/ (sum ne-l)
     (how-many ne-l)))

; NEList-of-temperatures -> Number
; computes the sum of the given temperatures 
(check-expect
  (sum (cons 1 (cons 2 (cons 3 '())))) 6)
(define (sum ne-l)
  (cond
    [(empty? (rest ne-l)) (first ne-l)]
    [else (+ (first ne-l) (sum (rest ne-l)))]))

; NEList-of-temperatures -> Number
; computes the number of temps in a list
(check-expect
  (how-many (cons 5 (cons 6 (cons 1 (cons 2 (cons 3 '())))))) 5)
(define (how-many ne-l)
  (cond
    [(empty? (rest ne-l)) 1]
    [else (+ 1 (how-many (rest ne-l)))]))

;  NEList-of-temperatures -> Boolean
; computes true if temperatire are sorted in descending order
(check-expect (sorted>? (cons 1 (cons 2 (cons 3 '())))) #false)
(check-expect (sorted>? (cons 5 (cons 5 (cons 3 (cons 2 (cons 1 '())))))) #true)
(define (sorted>? ne-l)
  (cond
    [(empty? (rest ne-l)) #true]
    [else (if (> (first (rest ne-l)) (first ne-l)) #false (sorted>? (rest ne-l)))]))
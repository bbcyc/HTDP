;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ex159b) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; List-of-string String -> N
; determines how often s occurs in los
(check-expect (count (cons "brian" (cons "is" (cons "brian" (cons "great" '())))) "brian") 2)

(define (count los s)
  (cond
    [(empty? los) 0]
    [else (if (string=? (first los) s) (+ 1 (count (rest los) s)) (count (rest los) s))]))


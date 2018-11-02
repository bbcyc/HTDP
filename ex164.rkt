;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ex164) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define US1 (cons 1 (cons 10 (cons 100 '()))))

; Number -> Number
; Convert one US$ amount to
; a Euro amount
(check-expect (convert1-euro 100) 81)
(define (convert1-euro a)
  (* a .81))

; Number Number -> Number
; takes an amount and a conversion rate
; and returns the converted amount
(check-expect (convert1-rate 100 .9) 90)
(define (convert1-rate a r)
  (* a r))


; List-of-Amounts -> List-of-Amounts
; converts a list of US$ amounts into
; a list of € amounts
(check-expect (convert-euro US1) (cons .81 (cons 8.1 (cons 81 '()))))
(define (convert-euro a)
  (cond
    [(empty? a) '()]
    [else (cons (convert1-euro (first a)) (convert-euro (rest a)))]))

(check-expect (convert-any US1 .9) (cons .9 (cons 9 (cons 90 '()))))
(define (convert-any a r)
  (cond
    [(empty? a) '()]
    [else (cons (convert1-rate (first a) r) (convert-any (rest a) r))]))

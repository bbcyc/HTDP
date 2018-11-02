;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname ex187) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define-struct gp [name score])
; A GamePlayer is a structure: 
;    (make-gp String Number)
; interpretation (make-gp p s) represents player p who 
; scored a maximum of s points 

(define UL1 (list (make-gp "brian" 10) (make-gp "james" 20) (make-gp "kyle" 30)))
(define OL1 (list (make-gp "kyle" 30) (make-gp "james" 20) (make-gp "brian" 10)))
(define UL2 (list (make-gp "brian" 20) (make-gp "chrissy" 10) (make-gp "kyle" 30)))
(define OL2 (list (make-gp "kyle" 30) (make-gp "brian" 20) (make-gp "chrissy" 10)))

; List-of-numbers -> List-of-numbers 
; rearranges alon in descending order
 
(check-expect (sort> UL1) OL1)
(check-expect (sort> UL2) OL2)
 
(define (sort> l)
  (cond
    [(empty? l) '()]
    [(cons? l) (insert (first l) (sort> (rest l)))]))

; Number List-of-numbers -> List-of-numbers
; inserts n into the sorted list of numbers alon

(check-expect (insert (make-gp "s" 5) '()) (list (make-gp "s" 5)))
(check-expect (insert (make-gp "b" 12) (list (make-gp "b" 20) (make-gp "b" -5)))
              (list (make-gp "b" 20) (make-gp "b" 12) (make-gp "b" -5)))

(define (insert gp l)
  (cond
    [(empty? l) (cons gp '())]
    [else (if (score>= gp (first l))
              (cons gp l)
              (cons (first l) (insert gp (rest l))))]))

; GamePlayer Gameplayer -> Boolean
; Returns true if the first GamePlayer
; has equal or more points than the second

(check-expect (score>= (make-gp "brian" 10) (make-gp "james" 20)) #false)
(check-expect (score>= (make-gp "brian" 20) (make-gp "james" 10)) #true)
(check-expect (score>= (make-gp "brian" 20) (make-gp "james" 20)) #true)

(define (score>= p1 p2)
  (>= (gp-score p1) (gp-score p2)))
;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ex213a) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; 1String LoW -> LoW
; Returns LoW created by inserting 1String into
; each position in each word of LoW
(check-expect (insert-everywhere/in-all-words "a" (list '())) (list (list "a")))
(check-expect (insert-everywhere/in-all-words "b" (list (list "a")))
              (list (list "a" "b") (list "b" "a")))
(check-expect (insert-everywhere/in-all-words "c" (list (list "a" "b") (list "b" "a")))
              (list (list "c" "a" "b") (list "a" "c" "b") (list "a" "b" "c")
                    (list "c" "b" "a") (list "b" "c" "a") (list "b" "a" "c")))
(define (insert-everywhere/in-all-words s low)
  (cond
    [(empty? low) (list (list s))]
    [else (append (insert-everywhere s (first low))
                (insert-everywhere/in-all-words s (rest low)))]))

; 1String Word -> LoW
; Returns LoW produced from inserting 1String
; into every position in Word
(check-expect (insert-everywhere "a" '()) (cons (cons "a" '()) '()))
(check-expect (insert-everywhere "b" (cons "a" '()))
              (list (list "a" "b") (list "b" "a")))
(check-expect (insert-everywhere "c" (list "a" "b"))
              (list (list "c" "a" "b") (list "a" "c" "b") (list "a" "b" "c")))

(define (insert-everywhere s w)
  (cond
    [(empty? w) (cons (cons s '()) '())]
    [else (cons (cons (first w) (cons s (insert-everywhere s (rest w)))) '())]))
    
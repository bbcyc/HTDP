;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname ex188) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define-struct email [from date message])
; An Email Message is a structure: 
;   (make-email String Number String)
; interpretation (make-email f d m) represents text m 
; sent by f, d seconds after the beginning of time 

(define UL1 (list (make-email "k" 10 "sup") (make-email "c" 30 "hey") (make-email "b" 20 "yo")))
(define OLT1 (list (make-email "c" 30 "hey") (make-email "b" 20 "yo") (make-email "k" 10 "sup")))
(define OLN1 (list (make-email "b" 20 "yo") (make-email "c" 30 "hey") (make-email "k" 10 "sup")))
(define E1 (make-email "d" 40 "mmm"))
(define E2 (make-email "z" 10 "no"))
(define E3 (make-email "n" 70 "shipit"))
(define UL2 (list E1 E2 E3))
(define OLT2 (list E3 E1 E2))
(define OLN2 (list E1 E3 E2))



; List-of-emails -> List-of-emails 
; rearranges aloe by time in descending order
 
(check-expect (sortbytime> UL1) OLT1)
(check-expect (sortbytime> UL2) OLT2)
 
(define (sortbytime> l)
  (cond
    [(empty? l) '()]
    [(cons? l) (insertt> (first l) (sortbytime> (rest l)))]))

; List-of-emails -> List-of-emails 
; rearranges aloe by from name in descending order
 
(check-expect (sortbyfrom< UL1) OLN1)
(check-expect (sortbyfrom< UL2) OLN2)
 
(define (sortbyfrom< l)
  (cond
    [(empty? l) '()]
    [(cons? l) (insertf< (first l) (sortbyfrom< (rest l)))]))

; Email List-of-emails -> List-of-emails
; inserts e into the sorted list of emails

(check-expect (insertt> E1 '()) (list E1))
(check-expect (insertt>  E1 (list E3 E2))
              (list E3 E1 E2))

(define (insertt> e l)
  (cond
    [(empty? l) (cons e '())]
    [else (if (etime>= e (first l))
              (cons e l)
              (cons (first l) (insertt> e (rest l))))]))

; Email List-of-emails -> List-of-emails
; inserts e into the sorted list of emails

(check-expect (insertf< E1 '()) (list E1))
(check-expect (insertf<  E3 (list E1 E2))
              (list E1 E3 E2))

(define (insertf< e l)
  (cond
    [(empty? l) (cons e '())]
    [else (if (efrom<= e (first l))
              (cons e l)
              (cons (first l) (insertf< e (rest l))))]))

; Email Email -> Boolean
; Returns true if the first email
; was sent after or the same time as the second

(check-expect (etime>= E1 E3) #false)
(check-expect (etime>= E1 E2) #true)
(check-expect (etime>= E1 E1) #true)

(define (etime>= e1 e2)
  (>= (email-date e1) (email-date e2)))

; Email Email -> Boolean
; Returns true if the first email from name
; is alphabetically before of the same as the second

(check-expect (efrom<= E1 E3) #true)
(check-expect (efrom<= E2 E1) #false)
(check-expect (efrom<= E1 E1) #true)

(define (efrom<= e1 e2)
  (string<=? (email-from e1) (email-from e2)))
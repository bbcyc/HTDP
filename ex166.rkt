;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ex166) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define-struct work [employee rate hours])
; A (piece of) Work is a structure: 
;   (make-work (make-employee String Number) Number Number)
; interpretation (make-work n r h) combines the name 
; with the pay rate r and the number of hours h

; Low (short for list of works) is one of: 
; – '()
; – (cons Work Low)
; interpretation an instance of Low represents the 
; hours worked for a number of employees

(define-struct employee [name number])

(define W1 '())
(define W2 (cons (make-work (make-employee "Robby" 1001) 11.95 39)
      '()))
(define W3 (cons (make-work (make-employee "Matthew" 1002) 12.95 45)
      (cons (make-work (make-employee "Robby" 1001) 11.95 39)
            '())))


(define-struct paycheck [employee amount])

; List-Of-Work -> List-of-Paychecks
(check-expect (wage*.v4 W3) (cons (make-paycheck (make-employee "Matthew" 1002) (* 12.95 45))
      (cons (make-paycheck (make-employee "Robby" 1001) (* 11.95 39))
            '())))
(define (wage*.v4 low)
  (cond
    [(empty? low) '()]
    [(cons? low) (cons (wage.v4 (first low))
                       (wage*.v4 (rest low)))]))

(define (wage.v4 w)
  (make-paycheck (make-employee (employee-name (work-employee w))
                                (employee-number (work-employee w)))
                 (* (work-rate w) (work-hours w))))



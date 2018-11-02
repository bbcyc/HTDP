;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex288) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define list1 (list 0 1 2 3))
(define list2 (list 1 2 3 4))
(define list3 (list 1 (/ 1 2) (/ 1 3) (/ 1 4)))
(define list4 (list 0 2 4 6))
(define list5 (list (list 1 0 0 0)
                    (list 0 1 0 0)
                    (list 0 0 1 0)
                    (list 0 0 0 1)))

; creates the list (list 0 ... (- n 1)) for any natural number n;
(check-expect (create-list1 4) list1)
(define (create-list1 n)
  (build-list n (lambda (j) (+ j 0))))

; creates the list (list 1 ... n) for any natural number n;
(check-expect (create-list2 4) list2)
(define (create-list2 n)
  (build-list n add1))

; creates the list (list 1 1/2 ... 1/n) for any natural number n;
(check-expect (create-list3 4) list3)
(define (create-list3 n)
  (build-list n (lambda (j) (/ 1 (add1 j)))))

; creates the list of the first n even numbers;
(check-expect (create-list4 4) list4)
(define (create-list4 n)
  (build-list n (lambda (j) (* 2 j))))

; creates a diagonal square of 0s and 1s;
(check-expect (create-list5 4) list5)
(define (create-list5 n)
  (build-list n
   (lambda (i)
     (build-list n
      (lambda (j) (if (= i j) 1 0)))))) 

; Also define tabulate with lambda.
; Number -> [List-of Number]
; tabulates sin between n 
; and 0 (incl.) in a list
(define (tab-sin n)
  (cond
    [(= n 0) (list (sin 0))]
    [else
     (cons
      (sin n)
      (tab-sin (sub1 n)))]))
  

; Number -> [List-of Number]
; tabulates sqrt between n 
; and 0 (incl.) in a list
(define (tab-sqrt n)
  (cond
    [(= n 0) (list (sqrt 0))]
    [else
     (cons
      (sqrt n)
      (tab-sqrt (sub1 n)))]))

; Number -> [List-of Number]
; tabulates a function g between n 
; and 0 (incl.) in a list
(check-within (tab-g 10 sin) (tab-sin 10) .01)
(check-within (tab-g 10 sqrt) (tab-sqrt 10) .01)
(define (tab-g n g)
  (cond
    [(= n 0) (list (g 0))]
    [else
     (cons
      (g n)
      (tab-g (sub1 n) g))]))

; Number -> [List-of Number]
; tabulates a function g between n 
; and 0 (incl.) in a list
; using lambda
(check-within (tabulate 10 sin) (tab-sin 10) .01)
(check-within (tabulate 10 sqrt) (tab-sqrt 10) .01)
(define (tabulate n f)
  (build-list (add1 n) (lambda (j) (f (- n j)))))
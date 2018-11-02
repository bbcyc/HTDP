;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname ex270) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; [X] N [N -> X] -> [List-of X]
; constructs a list by applying f to 0, 1, ..., (sub1 n)
; (build-list n f) == (list (f 0) ... (f (- n 1)))
; (define (build-list n f) ...)

; creates the list (list 0 ... (- n 1))
; for any natural number n;
(check-expect (zero-to-sub1-n 5) (list 0 1 2 3 4))
(define (zero-to-sub1-n n)
  (local (; Number -> Number
          ; returns number input
          (define (identity n) n))
    (build-list n identity)))

; creates the list (list 1 ... n) for any natural number n;
(check-expect (one-to-n 5) (list 1 2 3 4 5))
(define (one-to-n n)
    (build-list n add1))

; creates the list (list 1 1/2 ... 1/n)
; for any natural number n;
(check-expect (one-over-list 5) (list 1 (/ 1 2) (/ 1 3)
                                         (/ 1 4) (/ 1 5)))
(define (one-over-list n)
  (local (; Number -> Number
          ; returns one over (number plus one)
          (define (one-over n)
            (/ 1 (add1 n))))
    (build-list n one-over)))

; creates the list of the first n even numbers;
; Number -> List-of-Numbers
(check-expect (evens 5) (list 0 2 4 6 8))
(define (evens n)
  (local (; Number -> Number
          ; returns double the input
          (define (double n) (* 2 n)))
    (build-list n double)))

(define identity1 (list (list 1)))
(define identity2 (list (list 1 0) (list 0 1)))
(define identity3 (list (list 1 0 0) (list 0 1 0) (list 0 0 1)))
(check-expect (diagonal 2) identity2)
(check-expect (diagonal 3) identity3)
; creates a diagonal square of 0s and 1s
; Number -> List-of-List-of Numbers
(define (diagonal n)
  (local (; Number -> List-of-Numbers
          ; Returns a list of n digits
          ; with 1 at the m position
          ; and 0 elsewhere
          (define (create-row m)
            (local (; Number -> Number
                    ; returns 1 if m = i
                    ; 0 otherwise
                    (define (m-is-i? i)
                      (if (= m i) 1 0)))
              (build-list n m-is-i?))))
    (build-list n create-row)))

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

; Number -> [List-of-Number]
; tabulates a function between n and 0 on list
(define (tabulate n f)
  (cond
    [(= n 0) (list (f 0))]
    [else
     (cons
      (f n)
      (tabulate (sub1 n)))]))

(check-within (build-tab 5 sqrt) (tab-sqrt 5) 0.01)
(define (build-tab n f)
  (reverse (build-list (add1 n) f)))

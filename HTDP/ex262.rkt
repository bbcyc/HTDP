;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname ex262) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define identity1 (list (list 1)))
(define identity2 (list (list 1 0) (list 0 1)))
(define identity3 (list (list 1 0 0) (list 0 1 0) (list 0 0 1)))

; Number -> List-of-List-of-Numbers
; Returns diagonal line of 1's and all else 0's
;(check-expect (identityM 1) identity1)
(check-expect (identityM 2) identity2)
(check-expect (identityM 3) identity3)
(define (identityM m)
  (local (; Number -> List-of-Numbers
          ; Returns a list of numbers with the first
          ; first entry 1 followed by Number-1 0s
          (define (create-first-list m)
            (local ((define (first-row n)
                     (cond
                       [(= 0 n) 1]
                       [else 0])))
              (build-list m first-row)))

          ; List-of-List-of-Numbers -> List-of-List-of-Numbers
          ; returns a list of lists of numbers with 0
          ; appended to the first position of each list
          (define (add0-to-front lon)
            (cond
              [(empty? lon) '()]
              [else (cons (cons 0 (first lon))
                          (add0-to-front (rest lon)))])))
  (cond
    [(= m 1) (cons (cons 1 '()) '())]
    [else (cons (create-first-list m)
                (add0-to-front (identityM (sub1 m))))])))



  
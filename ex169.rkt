;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ex169) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define L1 (cons (make-posn 100 200)
                 (cons (make-posn 200 400)
                       (cons (make-posn 1 100)
                             (cons (make-posn -199 -543)
                                   (cons (make-posn 0 0)
                                         (cons (make-posn 59 122) '())))))))

(define L2 (cons (make-posn 100 200)
                 (cons (make-posn 1 100)
                       (cons (make-posn 0 0)
                             (cons (make-posn 59 122) '())))))

; List-of-Posns -> List-of-Posns
; returns LOP whose members contain posn-x in [0,100]
; and posn-y in [0, 200]
(check-expect (legal L1) L2)
(define (legal lop)
  (cond
    [(empty? lop) '()]
    [(and (<= 0 (posn-x (first lop)) 100)
          (<= 0 (posn-y (first lop)) 200))
     (cons (first lop) (legal (rest lop)))]
    [else (legal (rest lop))]))
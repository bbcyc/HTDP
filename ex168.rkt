;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ex168) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; List-of-Posns -> List-of-Posns
; (make-posn x y) -> (make-posn x (+ y 1)
(check-expect (translate (cons (make-posn 1 2)
                               (cons (make-posn 3 4)
                                     (cons (make-posn -5 -6) '()))))
              (cons (make-posn 1 3)
                    (cons (make-posn 3 5)
                          (cons (make-posn -5 -5) '()))))
(define (translate lop)
  (cond
    [(empty? lop) '()]
    [(cons? lop) (cons (make-posn (posn-x (first lop)) (+ (posn-y (first lop)) 1))
                       (translate (rest lop)))]))
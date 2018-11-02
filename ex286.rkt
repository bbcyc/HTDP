;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex286) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define-struct ir [name desc acq srp])
; LOIR -> LOIR
; sorts list of irs by price diff
(check-expect (price-diff (list (make-ir "a" "aa" 1 3)
                                (make-ir "b" "bb" 2 5)
                                (make-ir "c" "cc" 3 4)))
              (list (make-ir "b" "bb" 2 5)
                    (make-ir "a" "aa" 1 3)
                    (make-ir "c" "cc" 3 4)))
(define (price-diff loir)
  (sort loir (lambda (ir1 ir2) (> (- (ir-srp ir1) (ir-acq ir1))
                             (- (ir-srp ir2) (ir-acq ir2))))))
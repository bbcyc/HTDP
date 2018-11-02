;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname ex268) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define-struct ir [name desc acq srp])
; an inventory record IR is a struct:
; (make-ir String String Number Number)

; [X] [List-of X] [X X -> Boolean] -> [List-of X]
; produces a version of lx that is sorted according to cmp
; (define (sort lx cmp) ...)
; Number

; List-of-IRs -> List-of-IRs
; returns a list of IRs sorted by difference
; between the two prices
(check-expect (sort-by-diff-desc (list (make-ir
(define (sort-by-diff-desc loir) ...)

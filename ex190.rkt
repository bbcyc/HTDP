;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname ex190) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define LOS1 (list "a" "b" "c" "d"))
(define LOS2 (list "a" "b" "c"))
(define LOS3 (list "a" "b"))
(define LOS4 (list "a"))
(define LOS5 (list "b" "c" "d"))
(define LOS6 (list "c" "d"))
(define LOS7 (list "d"))

; List-of-1Strings -> List-of-lists-of-1Strings
; returns all prefixes from a list of 1Strings
(check-expect (prefixes LOS1) (list LOS1 LOS2 LOS3 LOS4))
(define (prefixes los)
  (cond
    [(empty? los) '()]
    [else (cons los (prefixes (drop-last los)))]))

; List-of-Strings -> List-of-Strings
; returns a LOS with the last non-empty
; element removed
(check-expect (drop-last LOS1) LOS2)
(define (drop-last los)
  (cond
    [(empty? los) '()]
    [(empty? (rest los)) '()]
    [else (cons (first los) (drop-last (rest los)))]))

; List-of-1Strings -> List-of-lists-of-1Strings
; returns all suffixes from a list of 1Strings
(check-expect (suffixes LOS1) (list LOS1 LOS5 LOS6 LOS7))
(define (suffixes los)
  (cond
    [(empty? los) '()]
    [else (cons los (suffixes (rest los)))]))

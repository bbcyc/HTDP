;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ex175) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/batch-io)

(define LLS1 (read-words/line "TTT.txt"))

(define-struct count [chars words lines])
; (make-count Number Number Number)
; represents the number of 1Strings, words, and lines
; in a file

; File -> Count
; BSL version of Unix command wc
; takes a file and returns the number of
; 1Strings, words, and lines in that file
(define (wc f)
  (get-count (read-words/line f)))

; LLS -> Count
; returns Count from a lls
(check-expect (get-count LLS1) (make-count 148 33 13))
(define (get-count lls)
  (make-count (number-chars lls) (number-words lls) (length lls)))

; LLS -> Number
; returns number of 1Strings in an LLS
(check-expect (number-chars LLS1) 148)
(define (number-chars lls)
  (cond
    [(empty? lls) 0]
    [else (+ (number-chars/line (first lls))
             (number-chars (rest lls)))]))

; LOS -> Number
; Takes a list of strings and returns the number of characters within
(define (number-chars/line los)
  (cond
    [(empty? los) 0]
    [else (+ (length (explode (first los)))
             (number-chars/line (rest los)))]))

; LLS -> Number
; returns number of words in an LLS
(check-expect (number-words LLS1) 33)
(define (number-words lls)
  (cond
    [(empty? lls) 0]
    [else (+ (length (first lls))
             (number-words (rest lls)))]))
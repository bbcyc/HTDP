;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex306) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/abstraction)

; LON -> LON
; converts a list of Euros to
; a list of dollars using for loops
(check-expect (convert-euro '(1 2 3)) '(1.06 2.12 3.18))
(define (convert-euro lod)
  (for/list ([dollar lod]) (* dollar 1.06)))
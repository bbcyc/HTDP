;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname ex196) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/batch-io)

(define DICTIONARY-LOCATION "C:/Users/robot/Documents/words.txt")

; A Dictionary is a List-of-strings.
(define DICTIONARY-AS-LIST (read-lines DICTIONARY-LOCATION))

(define TINYDICT (list "apple" "banana" "cherry" "currant" "dragonfruit"))

; A Letter is one of the following 1Strings: 
; – "a"
; – ... 
; – "z"
; or, equivalently, a member? of this list: 
(define LETTERS
  (explode "abcdefghijklmnopqrstuvwxyz"))

; Letter Dictionary -> Number
; Returns the number of words in a Dictionary
; starting with Letter

(check-expect (starts-with# "c" TINYDICT) 2)
(define (starts-with# l d)
  (cond
    [(empty? d) 0]
    [else (if (string=? l (first (explode (first d))))
              (add1 (starts-with# l (rest d)))
              (starts-with# l (rest d)))]))

(define-struct letter-count [letter count]) 
; A letter-count represents how many times a word beginning
; with a certain letter appears in a dictionary
; (make-letter-count Letter Number)

; Dictionary -> LetterCount

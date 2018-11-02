;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname dictionary) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/batch-io)

(define DICTIONARY-LOCATION "C:/Users/robot/Documents/words.txt")
 
; A Dictionary is a List-of-strings.
(define DICTIONARY-AS-LIST (read-lines DICTIONARY-LOCATION))

; A Letter is one of the following 1Strings: 
; – "a"
; – ... 
; – "z"
; or, equivalently, a member? of this list: 
(define LETTERS
  (explode "abcdefghijklmnopqrstuvwxyz"))

(define DICT1 (list "aa" "ab" "ac" "ba" "bb" "bc" "da" "db" "ea" "ff"))
(define LC1 (list (make-letter-count "a" 3)
                  (make-letter-count "b" 3)
                  (make-letter-count "d" 2)
                  (make-letter-count "e" 1)
                  (make-letter-count "f" 1)))
; Letter Dictionary -> Nunber
; Returns the number of words in Dictionary
; that start with Letter
(check-expect (starts-with# "a" '()) 0)
(check-expect (starts-with# "a" (list "aa")) 1)
(check-expect (starts-with# "a" (list "bb")) 0)
(check-expect (starts-with# "a" (list "aa" "bb")) 1)
(check-expect (starts-with#  "b" DICT1) 3)
(define (starts-with# l d)
  (cond
    [(empty? d) 0]
    [(string<? l (substring (first d) 0 1)) 0]
    [(string=? l (substring (first d) 0 1)) (add1 (starts-with# l (rest d)))]
    [else (starts-with# l (rest d))]))

(define-struct letter-count [letter count])
; A Letter-Count is a struct for keeping a
; count of how many words start with a certain letter
; (make-letter-count 1String Number)

; Dictionary -> List-of-LetterCounts
; Returns a list of lettercounts from a given dictionary
(check-expect (count-by-letter '()) '())
(check-expect (count-by-letter (list "aa")) (list (make-letter-count "a" 1)))
(check-expect (count-by-letter (list "aa" "ab")) (list (make-letter-count "a" 2)))
(check-expect (count-by-letter (list "aa" "bb"))
              (list (make-letter-count "a" 1) (make-letter-count "b" 1)))
(check-expect (count-by-letter DICT1) LC1)
(define (count-by-letter d)
  (cond
    [(empty? d) '()]
    [else ... (first d) ... (count-by-letter (rest d))]))

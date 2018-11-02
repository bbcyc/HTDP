;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname ex197) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
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

; List-of-Letters Dictionary -> LetterCount
; Returns a list of lettercounts corresponding to each letter
; in the list-of-letters provided and the dictionary

(check-expect (count-by-letter (list "a" "b" "c" "d") TINYDICT)
              (list (make-letter-count "a" 1) (make-letter-count "b" 1)
                    (make-letter-count "c" 2) (make-letter-count "d" 1)))

(define (count-by-letter lol d)
  (cond
    [(empty? lol) '()]
    [else (cons (make-letter-count (first lol)
                                   (starts-with# (first lol) d))
                (count-by-letter (rest lol) d))]))


; List-of-letter-counts -> List-of-letter-counts
; produces a sorted version of l
(define (sort> l)
  (cond
    [(empty? l) '()]
    [(cons? l) (insert (first l) (sort> (rest l)))]))
 
; LetterCount List-of-letter-counts -> List-of-letter-counts
; inserts n into the sorted list of lettercounts l 
(define (insert n l)
  (cond
    [(empty? l) (cons n '())]
    [else (if (>= (letter-count-count n) (letter-count-count (first l)))
              (cons n l)
              (cons (first l) (insert n (rest l))))]))

; Dictionary -> Letter-Count
; returns the Letter-Count for the letter than occurs
; most often in a given dictionary
(define (most-frequent d)
  (first (sort> (count-by-letter LETTERS d))))

(define (most-greatest d)
  (greatest (count-by-letter LETTERS d)))

; List-of-Letter-Count -> Letter-Count
; returns the largest letter-count from a list of letter-counts
(define (greatest llc)
  (cond
    [(empty? llc) '()]
    [(empty? (rest llc)) (first llc)]
    [else (if (> (letter-count-count (first llc)) (letter-count-count (first (rest llc))))
              (greatest (cons (first llc) (rest (rest llc))))
              (greatest (rest llc)))]))

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

(define-struct letter-count [letter count])
; A Letter-Count is a struct for keeping a
; count of how many words start with a certain letter
; (make-letter-count 1String Number)


(define DICT1 (list "aa" "ab" "ac" "ba" "bb" "bc" "da" "db" "dc" "de" "ea" "ff"))
(define LC1 (list (make-letter-count "a" 3)
                  (make-letter-count "b" 3)
                  (make-letter-count "d" 4)
                  (make-letter-count "e" 1)
                  (make-letter-count "f" 1)))
(define L1 (make-letter-count "a" 3))
(define L2 (make-letter-count "b" 5))
(define L3 (make-letter-count "c" 7))

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
    [(empty? (rest d)) (list (make-letter-count (substring (first d) 0 1) 1))]
    [else (if (string=? (substring (first d) 0 1) (substring (first (rest d)) 0 1))
              (add1-to-lc (count-by-letter (rest d)))
              (cons (make-letter-count (substring (first d) 0 1) 1)
                    (count-by-letter (rest d))))]))

; List-of-LetterCounts -> List-of-LetterCounts
; Adds 1 to the count of a the first lettercount
; in a list of lettercounts
(define (add1-to-lc llc)
  (cons (make-letter-count (letter-count-letter (first llc))
                          (add1 (letter-count-count (first llc))))
        (rest llc)))


(define (count-by-letter-v2 l d)
  (get-letter-counts LETTERS d))

; List-of-Letters Dictionary -> List-of-Lettercounts
; returns the list of lettercounts for a given list of letters
; and dictionary
(define (get-letter-counts l d)
   (cond
    [(empty? l) '()]
    [else (cons (make-letter-count (first l) (starts-with# (first l) d))
                (count-by-letter-v2 (rest l) d))]))
  
; Dictionary -> LetterCount
; Returns the lettercount for the letter
; that most frequently starts words in a
; given dictionary
(check-expect (most-frequent DICT1) (third LC1))
(define (most-frequent d)
  (get-largest-lc (count-by-letter d)))

; LoLC -> LC
; Returns the largest lettercount
; from a list of lettercounts
(check-expect (get-largest-lc LC1) (third LC1)) 
(define (get-largest-lc lolc)
  (cond
    [(empty? (rest lolc)) (first lolc)]
    [else (if (lc>? (first lolc) (second lolc))
         (get-largest-lc (cons (first lolc) (rest (rest lolc))))
         (get-largest-lc (rest lolc)))]))

; LC LC -> Boolean
; Returns #true if the first LC has a
; larger count than the second
(define (lc>? lc1 lc2)
  (if (> (letter-count-count lc1) (letter-count-count lc2)) #true #false))

; Dictionary -> LetterCount
; Returns largest lettercount from
; a given dictionary using sort method
(check-expect (most-frequent-sort DICT1) (third LC1))
(define (most-frequent-sort d)
  (first (sort-lc> (count-by-letter d))))

; LoLC -> LoLC
; Returns a LoLC sorted by count descending
; from a given LoLC
(define (sort-lc> lolc)
  (cond
    [(empty? lolc) '()]
    [else (insert-lc (first lolc) (sort-lc> (rest lolc)))]))

; LetterCount LoLC -> LoLC
; inserts a lettercount into a sorted list of lettercounts
(check-expect (insert-lc L1 '()) (list L1))

(check-expect (insert-lc L2 (list L3 L1)) (list L3 L2 L1))
(define (insert-lc lc lolc)
  (cond
    [(empty? lolc) (cons lc '())]
    [else (if (lc>? lc (first lolc))
              (cons lc lolc)
              (cons (first lolc) (insert-lc lc (rest lolc))))]))


; List-of-Words -> List-of-lists-of-words
; Returns a list of list of words where the
; words in each list of words start with the
; same letter
(check-expect (words-by-first-letter '()) '())
(check-expect (words-by-first-letter (cons "apple" '()))
              (cons (cons "apple" '()) '()))
(check-expect (words-by-first-letter (cons "aa" (cons "ab" '())))
              (cons (cons "aa" (cons "ab" '())) '()))
(check-expect (words-by-first-letter (cons "aa" (cons "bb" '())))
              (cons (cons "aa" '()) (cons (cons "bb" '()) '())))
(check-expect (words-by-first-letter (cons "aa" (cons "ab" (cons "bb" '()))))
              (cons (cons "aa" (cons "ab" '())) (cons (cons "bb" '()) '())))
(define (words-by-first-letter d)
  (cond
    [(empty? d) '()]
    [(empty? (rest d)) (cons d '())]
    [else (if (1st-letter=? (first d) (second d))
              (insert-word (first d) (words-by-first-letter (rest d)))
              (cons (list (first d)) (words-by-first-letter (rest d))))]))

; String String -> Boolean
; Returns true if both strings start with the same letter
(check-expect (1st-letter=? "aa" "ab") #true)
(check-expect (1st-letter=? "aa" "ba") #false)
(define (1st-letter=? s1 s2)
  (if (string=? (substring s1 0 1) (substring s2 0 1)) #true #false))

; String LoLoW -> LoLoW
; Inserts String at beginning of the first list
; of a list-of-list-of-words
(check-expect (insert-word "aa" (cons (cons "ab" '()) '()))
              (cons (cons "aa" (cons "ab" '())) '()))
(define (insert-word s lolow)
  (cons (cons s (first lolow)) (rest lolow)))

; List-of-Words -> LetterCount
; Given a list of words, return the lettercount
; with the largest count
(check-expect (most-frequent.v2 DICT1) (third LC1))
(check-expect (most-frequent DICTIONARY-AS-LIST)
              (most-frequent.v2 DICTIONARY-AS-LIST))
(define (most-frequent.v2 d)
  (get-longest-list-lc (words-by-first-letter d)))

; LoLoW -> LetterCount
; Returns the LC for the longest list
; in a list of list of wprds
(check-expect (get-longest-list-lc (words-by-first-letter DICT1)) (third LC1))
(define (get-longest-list-lc lolow)
  (cond
    [(empty? (rest lolow)) (lc-from-low (first lolow))]
    [else (if (> (length (first lolow)) (length (second lolow)))
              (get-longest-list-lc (cons (first lolow) (rest (rest lolow))))
              (get-longest-list-lc (rest lolow)))]))

; LoW -> LC
; Returns a LetterCount for a LoW
; that start with the same letter
(check-expect (lc-from-low (list "aa" "ab" "ac" "ad"))
                           (make-letter-count "a" 4))
(define (lc-from-low low)
  (make-letter-count (substring (first low) 0 1) (length low)))
  
;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname ex198) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/batch-io)

; (define DICTIONARY-LOCATION "C:/Users/robot/Documents/words.txt")
(define DICTIONARY-LOCATION "/usr/share/dict/words")

; A Dictionary is a List-of-strings.
(define DICTIONARY-AS-LIST (read-lines DICTIONARY-LOCATION))

(define TINYDICT (list "apple" "banana" "cherry" "currant" "dragonfruit"))

(define DICT (list "a" "aa" "ab" "b" "ba" "bb" "bc" "c" "ca" "cb" "cc" "cd" "ff"))
(define LODICT (list (list "a" "aa" "ab")
                    (list "b" "ba" "bb" "bc")
                    (list "c" "ca" "cb" "cc" "cd")
                    (list "ff")))
(define LOLETTERS (explode "abcdef"))
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
    [(string<? l (substring (first d) 0 1)) 0]
    [else (if (string=? l (substring (first d) 0 1))
              (add1 (starts-with# l (rest d)))
              (starts-with# l (rest d)))]))

(define-struct letter-count [letter count]) 
; A letter-count represents how many times a word beginning
; with a certain letter appears in a dictionary
; (make-letter-count Letter Number)

; List-of-Letters Dictionary -> LetterCount
; Returns a list of lettercounts corresponding to each letter
; in the list-of-letters provided and the dictionary

(define LOLETTERCOUNT (list 
 (make-letter-count "a" 3)
 (make-letter-count "b" 4)
 (make-letter-count "c" 5)
 (make-letter-count "d" 0)
 (make-letter-count "e" 0)
 (make-letter-count "f" 1)))

(define SORTEDLOLETTERCOUNT (list 
 (make-letter-count "c" 5)
 (make-letter-count "b" 4)
 (make-letter-count "a" 3)
 (make-letter-count "f" 1)
 (make-letter-count "d" 0)
 (make-letter-count "e" 0)
 ))

(define SORTEDLOLETTERCOUNTG (list 
 (make-letter-count "c" 5)
 (make-letter-count "b" 4)
 (make-letter-count "a" 3)
 (make-letter-count "g" 2)
 (make-letter-count "f" 1)
 (make-letter-count "d" 0)
 (make-letter-count "e" 0)
 ))

(check-expect (count-by-letter (list "a" "b" "c" "d") TINYDICT)
              (list (make-letter-count "a" 1) (make-letter-count "b" 1)
                    (make-letter-count "c" 2) (make-letter-count "d" 1)))
(check-expect (count-by-letter LOLETTERS DICT) LOLETTERCOUNT)
              
(define (count-by-letter lol d)
  (cond
    [(empty? lol) '()]
    [else (cons (make-letter-count (first lol)
                                   (starts-with# (first lol) d))
                (count-by-letter (rest lol) d))]))


; List-of-letter-counts -> List-of-letter-counts
; produces a sorted version of l
(check-expect (sort> LOLETTERCOUNT) SORTEDLOLETTERCOUNT)
(define (sort> l)
  (cond
    [(empty? l) '()]
    [(cons? l) (insert (first l) (sort> (rest l)))]))
 
; LetterCount List-of-letter-counts -> List-of-letter-counts
; inserts n into the sorted list of lettercounts l
(check-expect (insert (make-letter-count "g" 2) SORTEDLOLETTERCOUNT) SORTEDLOLETTERCOUNTG)
(define (insert n l)
  (cond
    [(empty? l) (cons n '())]
    [else (if (>= (letter-count-count n) (letter-count-count (first l)))
              (cons n l)
              (cons (first l) (insert n (rest l))))]))

; Dictionary -> Letter-Count
; returns the Letter-Count for the letter than occurs
; most often in a given dictionary

(check-expect (most-frequent DICT) (most-greatest DICT))
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

; List-of-Words -> List-of-List-of-Words
; Turns a dictionary into a list of dictionaries, one per letter
(check-expect (words-by-first-letter (list "a")) (list (list "a")))
(check-expect (words-by-first-letter (list "a" "b")) (list (list "a") (list "b")))
(check-expect (words-by-first-letter (list "aa" "ab" "b" "bc")) (list (list "aa" "ab") (list "b" "bc")))

(check-expect (words-by-first-letter DICT) LODICT)
(define (words-by-first-letter d)
   (cond
    [(empty? (rest d)) (cons d '())]
    [(string=? (first-letter (first d)) (first-letter (second d)))
     (insert-word (first d) (words-by-first-letter (rest d)))]
    [else (cons (list (first d)) (words-by-first-letter (rest d)))]))

; Word List-of-List-of-Words -> LoLoW
; Inserts a word into the first list of list of words
(check-expect (insert-word "bri" (list (list "aa" "ab"))) (list (list "bri" "aa" "ab")))
(define (insert-word w lolow)
  (cons (cons w (first lolow)) (rest lolow)))



; Letter Dictionary -> Dictionary
; returns Dictionary containing all words
; that start with Letter
(check-expect (get-all-for-letter "a" '()) '())
(check-expect (get-all-for-letter "c" TINYDICT) (list "cherry" "currant"))
(define (get-all-for-letter l d)
  (cond
    [(empty? d) '()]
    [(string>? (first-letter (first d)) l) '()]
    [else (if (string=? (first-letter (first d)) l)
              (cons (first d) (get-all-for-letter l (rest d)))
              (get-all-for-letter l (rest d)))]))

; String -> 1String
; returns first letter in a string
(define (first-letter s)
  (substring s 0 1))

(check-expect
  (most-frequent DICTIONARY-AS-LIST)
  (most-frequent.v2 DICTIONARY-AS-LIST))

; LoLoW -> LetterCount
; Given a LoLoW return the LetterCount for the
;letter that starts the most words in the LoLoW
(define (most-frequent.v2 d)
  (first (sort> (get-letter-count-v2 d))))

; LoLoW -> LoLC
; Returns a LoLC for a given LoLoW
(define (get-letter-count-v2 lolow)
  (cond
    [(empty? lolow) '()]
    [else (cons (get-count-single (first-letter (first (first lolow))) (first lolow))
                (get-letter-count-v2 (rest lolow)))]))

; Letter LoLoW -> LetterCount
; returns the lettercount for one letter for a LoLoW
(define (get-count-single l lolow)
  (cond
    [(empty? lolow) (make-letter-count l 0)]
    [(string=? l (first-letter (first (first lolow)))) (make-letter-count l (length (first lolow)))]
    [else (get-count-single l (rest lolow))]))


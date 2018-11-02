;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname ex212) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/batch-io)

(define DICTIONARY-LOCATION "C:/Users/robot/Documents/words.txt")

; A Dictionary is a List-of-strings.
(define DICTIONARY (read-lines DICTIONARY-LOCATION))

; List-of-strings -> Boolean
(define (all-words-from-rat? w)
  (and
    (member? "rat" w) (member? "art" w) (member? "tar" w)))
 
; String -> List-of-strings
; finds all words that the letters of some given word spell
 
(check-member-of (alternative-words "cat")
                 (list "act" "cat")
                 (list "cat" "act"))
 
(check-satisfied (alternative-words "rat")
                 all-words-from-rat?)
 
(define (alternative-words s)
  (in-dictionary
    (words->strings (arrangements (string->word s)))))
 
; List-of-words -> List-of-strings
; turns all Words in low into Strings 
(define (words->strings low)
  (cond
    [(empty? low) '()]
    [else (cons (word->string (first low))
                (words->strings (rest low)))]))
 
; List-of-strings -> List-of-strings
; picks out all those Strings that occur in the dictionary 
(define (in-dictionary los)
  (cond
    [(empty? los) '()]
    [(member? (first los) DICTIONARY)
     (cons (first los) (in-dictionary (rest los)))]
    [else (in-dictionary (rest los))]))
     

; A Word is one of:
; – '() or
; – (cons 1String Word)
; interpretation a Word is a list of 1Strings (letters)
 
; A List-of-words is one of:
; - '() or
; - (cons Word List-of-Words)
; interpretation a List-of-Words is a
; list of list of 1Strings


 
; Word -> List-of-words
; finds all rearrangements of word
(define (arrangements word)
  (list word))

; String -> Word
; converts s to the chosen word representation 
(define (string->word s)
  (explode s))
 
; Word -> String
; converts w to a string
(define (word->string w)
  (implode w))

; 1String LoW -> LoW
; Returns a LoW where 1String has
; been inserted into every position
; of every word
(check-expect (insert-everywhere/in-all-words "a" (list '()))
              (list (list "a")))
(check-expect (insert-everywhere/in-all-words "b" (list (list "a")))
              (list (list "a" "b") (list "b" "a")))
(check-expect (insert-everywhere/in-all-words "c" (list (list "a" "b") (list "b" "a")))
              (list (list "a" "b" "c") (list "a" "c" "b") (list "b" "c" "a")
                    (list "b" "a" "c") (list "c" "b" "a") (list "c" "a" "b")))
(define (insert-everywhere/in-all-words s low)
  (cond
    [(empty? low) (list (list s))]
    [else (cons (insert-everywhere s (first low))
                (insert-everywhere/in-all-words s (rest low)))]))

; 1String Word -> LoW
; Returns LoW produced from inserting 1String
; into every position in Word
(check-expect (insert-everywhere "a" '()) (cons (cons "a" '()) '()))
(check-expect (insert-everywhere "b" (cons "a" '()))
              (list (list "a" "b") (list "b" "a")))
(check-expect (insert-everywhere "c" (list "a" "b"))
              (list (list "c" "a" "b") (list "a" "c" "b") (list "a" "b" "c")))

(define (insert-everywhere s w)
  (cond
    [(empty? w) (cons (cons s '()) '())]
    [else (cons (cons (first w) (cons s (insert-everywhere s (rest w)))) '())]))
    
    










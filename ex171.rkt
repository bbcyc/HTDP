;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ex171) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/batch-io)

; List-of-strings is one of the following:
; - '()
; (cons String List-of-Strings)

; List-of-List-of-Strings is one of the following:
; - '()
; (cons List-of-Strings LLS)

; String -> String
; produces the content of file f as a string 
; (define (read-file f) ...)
 
; String -> List-of-string
; produces the content of file f as a list of strings, 
; one per line
; (define (read-lines f) ...)
 
; String -> List-of-string
; produces the content of file f as a list of strings, 
; one per word
; (define (read-words f) ...)
 
; String -> List-of-list-of-string
; produces the content of file f as a list of list of
; strings, one list per line and one string per word 
; (define (read-words/line f) ...)
 
; The above functions consume the name of a file as a String
; argument. If the specified file does not exist in the 
; same folder as the program, they signal an error.

(define line0 (cons "hello" (cons "world" '())))
(define line1 '())
 
(define lls0 '())
(define lls1 (cons line0 (cons line1 '())))

(define (line-processor ln)
  (cond
    [(empty? ln) 0]
    [else
     (+ 1 (line-processor (rest ln)))]))


; LLS -> List-of-numbers
; determines the number of words on each line

(check-expect (words-on-line lls0) '())
(check-expect (words-on-line lls1)
              (cons 2 (cons 0 '())))

(define (words-on-line lls)
  (cond
    [(empty? lls) '()]
    [else
     (cons (line-processor (first lls))
           (words-on-line (rest lls)))]))

; String -> List-of-numbers
; counts the words on each line in the given file
(define (file-statistic file-name)
  (words-on-line
    (read-words/line file-name)))

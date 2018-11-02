;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ex174) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/batch-io)

; 1String -> String
; converts the given 1String to a 3-letter numeric String
 
(check-expect (encode-letter "z") (code1 "z"))
(check-expect (encode-letter "\t")
              (string-append "00" (code1 "\t")))
(check-expect (encode-letter "a")
              (string-append "0" (code1 "a")))
 
(define (encode-letter s)
  (cond
    [(>= (string->int s) 100) (code1 s)]
    [(< (string->int s) 10)
     (string-append "00" (code1 s))]
    [(< (string->int s) 100)
     (string-append "0" (code1 s))]))
 
; 1String -> String
; converts the given 1String into a String
 
(check-expect (code1 "z") "122")
 
(define (code1 c)
  (number->string (string->int c)))

; File -> File
; converts a text file into a numerically encoded file
(define (encode-file f)
  (write-file (string-append "encoded-" f)
              (encode-lines (read-words/line f))))

; LLS -> String
; converts an LLS in one numerically encoded string
(define (encode-lines lls)
  (cond
    [(empty? lls) ""]
    [else (string-append (encode-line (first lls))
                         (encode-letter "\n")
                         (encode-lines (rest lls)))]))

; List-of-strings -> String
; convert a list of strings into one numerically encoded string
(define (encode-line los)
  (cond
    [(empty? los) ""]
    [else (string-append (encode-word (first los))
                         (encode-letter " ")
                         (encode-line (rest los)))]))

; String -> String
; encodes a word into a numerically encoded string
(define (encode-word w)
  (cond
    [(string? w) (encode-word (explode w))]
    [(empty? w) ""]
    [else (string-append (encode-letter (first w))
                         (encode-word (rest w)))]))
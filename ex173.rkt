;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ex173) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
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


; LLS -> List-of-numbers
; determines the number of words on each line

(check-expect (words-on-line lls0) '())
(check-expect (words-on-line lls1)
              (cons 2 (cons 0 '())))

(define (words-on-line lls)
  (cond
    [(empty? lls) '()]
    [else
     (cons (length (first lls))
           (words-on-line (rest lls)))]))

; String -> List-of-numbers
; counts the words on each line in the given file
(define (file-statistic file-name)
  (words-on-line
    (read-words/line file-name)))

; List-of-Strings -> String
; converts a list of strings into
; a single string separated by " "
(define (process-line los)
  (cond
    [(empty? los) ""]
    [(empty? (rest los)) {first los}]
    [else (string-append (first los) " " (process-line (rest los)))]))
    
; LLS -> String
; converts a list-of-lines into a string
; strings separated by " ", lines by "\n"
(define (collapse lls)
  (cond
    [(empty? lls) ""]
    [else (string-append (process-line (first lls))
                         "\n" (collapse (rest lls)))]))

; LOS -> Sring
; Removes all articles from a list of strings
(define (remove-from-line los)
  (cond
    [(empty? los) ""]
    [(or (string=? (first los) "a")
         (string=? (first los) "an")
         (string=? (first los) "the")) (remove-from-line (rest los))]
    [(empty? (rest los)) (first los)]
    [else (string-append (first los) " " (remove-from-line (rest los)))]))

; LLS->String
; takes a list of list of strings 
; and removes the articles from it
(define (process-file lls)
  (cond
    [(empty? lls) ""]
    [else (string-append (remove-from-line (first lls))
                         "\n" (process-file (rest lls)))]))
  

; File -> File
; Given a file, remove the article and write to another file
(define (remove-articles f)
  (write-file (process-file (read-words/line f))))
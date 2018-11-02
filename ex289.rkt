;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex289) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define list1 (list "aaa" "bbb" "ccc" "ddd"))

; String List-of-Strings -> Boolean
; Returns #true if any String in the LOS is
; equal to or an extension of the given string
(check-expect (find-name "aaa" list1) #true)
(check-expect (find-name "zzz" list1) #false)
(check-expect (find-name "bb" list1) #true)
(define (find-name s los)
  (ormap
   (lambda (p)
     (cond
       [(string=? p s) #true]
       [(and (> (string-length p) (string-length s))
             (string=? (substring p 0 (string-length s)) s)) #true]
       [else #false]))
   los))
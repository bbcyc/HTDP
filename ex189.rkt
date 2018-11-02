;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname ex189) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; Number List-of-numbers -> Boolean
(define (search n alon)
  (cond
    [(empty? alon) #false]
    [else (or (= (first alon) n)
              (search n (rest alon)))]))

(define SLON1 (list 1 3 5 7 9 11))
; Number List-of-numbers -> Boolean
(check-expect (search-sorted 7 SLON1) #true)
(check-expect (search-sorted 6 SLON1) #false)
(define (search-sorted n slon)
  (cond
    [(empty? slon) #false]
    [(< n (first slon)) #false]
    [else (or (= (first slon) n)
              (search-sorted n (rest slon)))]))

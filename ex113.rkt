;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ex113) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(check-expect (missile-or-not? #false) #true)
(check-expect (missile-or-not? (make-posn 9 2)) #true)
(check-expect (missile-or-not? "yellow") #false)
(check-expect (missile-or-not? #true) #false)
(check-expect (missile-or-not? 10) #false)
(check-expect (missile-or-not? empty-image) #false)

(define (missile-or-not? v)
  (or (false? v) (posn? v)))

(define (SIGS-or-not? s)
  (and (posn? (sigs-ufo s))
       (tank? (sigs-tank s))
       (missile-or-not? (sigs-missile s))))

(define (coordinate-or-not? n)
  (or (number? n) (posn? n)))

(define (vanimal-or-not? v)
  (or (vcat? v) (vcham? v)))

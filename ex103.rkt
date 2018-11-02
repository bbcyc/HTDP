;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ex103) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define-struct zoo_animal[type])
; A Zoo_Animal is one of:
; - spider
; - elephant
; - boa_constrictor
; - armadillo

(define-struct spider[legs vol])
; A Spider is a struct that takes
; two numbers, the number of legs
; and the volume of the cage is requires
; (make-spider Number Number)

(define-struct elephant[vol])
; An Elepant is a struct that take
; one number representin the volume
; of its cage
; (make-elephant vol)

(define-struct boa[length girth vol])
; A boa is a struct that takes three Numbers
; length for the boa's length
; girth for the boa's girth
; vol for the volume of the cage for transport
; (make-boa Number Number Number)

(define-struct armadillo[bands vol])
; An Armadillo is a struct that takes
; two numbers. bands represemts the number of
; bands or stripes the armadillo has
; vol represents the volume of the transport cage
; (make-armadillo Number Number)

; ZooAnimal Number -> Boolean
; takes a zoo animal and a number representing
; the volume of a cage and returns a boolean
; representing whether the animal will fit in
; a cage of that volumer
(define (fits? a cage)
  (cond
    [(spider? a) (>= cage (spider-vol a))]
    [(elepant? a) (>= cage (elephant-vol a))]
    [(boa? a) (>= cage (boa-vol a))]
    [(armadillo? a) (>= cage (armadillo-vol a))]))
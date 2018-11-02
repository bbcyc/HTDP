;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname ex267) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; List-of-Numbers -> List-of-Numbers
; converts a list of US$ amounts to a list of Euro amounts
; exchange rate $1.06/Euro
(check-expect (convert-euro (list 1.06 2.12 3.18)) (list 1 2 3))
(define (convert-euro lod)
  (local (; Number -> Number
          ; convert a dollar amount to a Euro amount
          (define (US-to-Euro dollar) (/ dollar 1.06)))
    (map US-to-Euro lod)))

; List-of-Temps -> List-of-Temps
; converts a list of fahr temps to a
; list of Celsius temps
(check-expect (convertFC (list 32 212)) (list 0 100))
(define (convertFC lot)
  (local (; convert a fahr temp to a cel one
          (define (f-to-c temp) (/ (* (- temp 32) 5) 9)))
    (map f-to-c lot)))

; List-of-Posns -> List-of-Pairs-of-Numbers
; converts a list of posns to a list of
; pairs of numbers
(check-expect (translate (list (make-posn 1 2) (make-posn 3 4) (make-posn 5 6))) (list (list 1 2) (list 3 4) (list 5 6)))
(define (translate lop)
  (local (; convert a posn into a list of numbers
          (define (posn-to-list p)
            (list (posn-x p) (posn-y p))))
    (map posn-to-list lop)))

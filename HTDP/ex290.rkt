;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex290) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)

(define list1 (list 1 2 3 4))
(define list2 (list 5 6 7 8))
(define list3 (list 1 2 3 4 5 6 7 8))
(define list4 (list (circle 10 "solid" "red")
                    (circle 20 "solid" "red")
                    (circle 30 "solid" "red")))
                    
; List List -> List
; Appends one list to another
(check-expect (append-from-fold list1 list2) list3)
(define (append-from-fold l1 l2)
  (foldr cons l2 l1))

(check-expect (sum-from-fold list1) 10)
(define (sum-from-fold lon)
  (foldr + 0 lon))

(check-expect (product-from-fold list1) 24)
(define (product-from-fold lon)
  (foldr * 1 lon))

; List-of-Images -> Image
; Horizontally composes a list of images
(check-expect (hor-images list4) (beside (circle 10 "solid" "red")
                                          (circle 20 "solid" "red")
                                          (circle 30 "solid" "red")
                                          empty-image))
(define (hor-images loi)
  (foldr beside empty-image loi))

; List-of-Images -> Image
; Veritcally composes a list of images
(check-expect (vert-images list4) (above (circle 10 "solid" "red")
                                          (circle 20 "solid" "red")
                                          (circle 30 "solid" "red")
                                          empty-image))
(define (vert-images loi)
  (foldr above empty-image loi))
;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ex165) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define L1 (cons "robot" (cons "car" (cons "paint" (cons "robot" (cons "lego" '()))))))
(define L2 (cons "r2d2" (cons "car" (cons "paint" (cons "r2d2" (cons "lego" '()))))))

; List-of-1Strings -> List-of-1Strings
; find and replace all occurrence of the string
; "robot" with "r2d2"
(check-expect (subst-robot L1) L2)
(define (subst-robot los)
  (cond
    [(empty? los) '()]
    [(string=? "robot" (first los)) (cons "r2d2" (subst-robot (rest los)))]
    [else (cons (first los) (subst-robot (rest los)))]))

; String String List-of-Strings -> List-of-Strings
; replace all occurrence of "old" string with "new"
; string in a list of strings
(check-expect (substitute "robot" "r2d2" L1) L2)
(define (substitute old new los)
  (cond
    [(empty? los) '()]
    [(string=? old (first los)) (cons new (subst-robot (rest los)))]
    [else (cons (first los) (subst-robot (rest los)))]))
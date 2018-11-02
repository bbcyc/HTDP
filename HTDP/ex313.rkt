;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex313) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define-struct no-parent [])
(define-struct child [father mother name date eyes])
; An FT (short for family tree) is one of: 
; – (make-no-parent)
; – (make-child FT FT String N String)

(define NP (make-no-parent))
; An FT is one of: 
; – NP
; – (make-child FT FT String N String)

; Oldest Generation:
(define Carl (make-child NP NP "Carl" 1926 "green"))
(define Bettina (make-child NP NP "Bettina" 1926 "green"))
 
; Middle Generation:
(define Adam (make-child Carl Bettina "Adam" 1950 "hazel"))
(define Dave (make-child Carl Bettina "Dave" 1955 "black"))
(define Eva (make-child Carl Bettina "Eva" 1965 "blue"))
(define Fred (make-child NP NP "Fred" 1966 "pink"))
 
; Youngest Generation: 
(define Gustav (make-child Fred Eva "Gustav" 1988 "brown"))

; FT -> Number
; Returns the number of child structures
; in a given family-tree
(check-expect (count-persons Carl) 1)
(check-expect (count-persons Adam) 3)
(check-expect (count-persons Gustav) 5)
(define (count-persons an-ftree)
  (cond
    [(no-parent? an-ftree) 0]
    [else (add1 (+ (count-persons (child-father an-ftree))
             (count-persons (child-mother an-ftree))))]))

; FT Number -> Number
; Returns the average age of all child structures
; given the current year and a family tree
(check-expect (average-age Carl 2018) 92)
(check-expect (average-age Adam 2018) 84)
(check-expect (average-age Gustav 2018) 63.8)
(define (average-age an-ftree year)
  (local ((define (sum-ages an-ftree year)
  (cond
    [(no-parent? an-ftree) 0]
    [else (+ (- year (child-date an-ftree))
             (sum-ages (child-father an-ftree) year)
             (sum-ages (child-mother an-ftree) year))])))
  (/ (sum-ages an-ftree year) (count-persons an-ftree))))

; FT -> List-of-eye-colors
; Returns the list of all eye colors
; in a given family tree
(check-expect (eye-colors Carl) '("green"))
(check-expect (eye-colors Adam) '("hazel" "green" "green"))
(check-expect (eye-colors Gustav) '("brown" "pink" "blue" "green" "green"))
(define (eye-colors an-ftree)
  (cond
    [(no-parent? an-ftree) '()]
    [else (append (list (child-eyes an-ftree))
                  (append (eye-colors (child-father an-ftree))
                          (eye-colors (child-mother an-ftree))))]))

; FT -> Boolean
(check-expect (blue-eyed-ancestor? Eva) #false)
(check-expect (blue-eyed-ancestor? Gustav) #true)
(define (blue-eyed-ancestor? an-ftree)
  (cond
    [(or (no-parent? (child-father an-ftree))
         (no-parent? (child-mother an-ftree))) #false]
    [else
     (or
       (string=? (child-eyes (child-father an-ftree)) "blue")
       (string=? (child-eyes (child-mother an-ftree)) "blue")
       (blue-eyed-ancestor?
         (child-father an-ftree))
       (blue-eyed-ancestor?
         (child-mother an-ftree)))]))

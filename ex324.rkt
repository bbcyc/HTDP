;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex324) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define-struct no-info [])
(define NONE (make-no-info))
 
(define-struct node [ssn name left right])
; A BT (short for BinaryTree) is one of:
; – NONE
; – (make-node Number Symbol BT BT)

(define b10 (make-node 10 "a" NONE NONE))
(define b24 (make-node 24 "b" NONE NONE))
(define b15 (make-node 15 "c" b10 b24))
(define b29 (make-node 29 "d" b15 NONE))
(define b77 (make-node 77 "e" NONE NONE))
(define b99 (make-node 99 "f" NONE NONE))
(define b95 (make-node 95 "g" NONE b99))
(define b89 (make-node 89 "h" b77 b95))
(define b63 (make-node 63 "i" b29 b89))

; Number BT -> Boolean
; returns true if a given number
; appears in a given binary tree
(check-expect (contains-bt? 10 b10) #true)
(check-expect (contains-bt? 10 b15) #true)
(check-expect (contains-bt? 10 b29) #true)
(check-expect (contains-bt? 10 b63) #true)
(check-expect (contains-bt? 10 b89) #false)
(check-expect (contains-bt? 10 b95) #false)
(define (contains-bt? n bt)
  (cond
    [(no-info? bt) #false]
    [(= n (node-ssn bt)) #true]
    [else (or (contains-bt? n (node-left bt))
              (contains-bt? n (node-right bt)))]))

(check-expect (search-bt? 10 b10) "a")
(check-expect (search-bt? 10 b15) "a")
(check-expect (search-bt? 10 b29) "a")
(check-expect (search-bt? 10 b63) "a")
(check-expect (search-bt? 10 b89) #false)
(check-expect (search-bt? 10 b95) #false)
(define (search-bt? n bt)
  (cond
    [(no-info? bt) #false]
    [(= n (node-ssn bt)) (node-name bt)]
    [(boolean? (search-bt? n (node-left bt))) (search-bt? n (node-right bt))]
    [(boolean? (search-bt? n (node-right bt))) (search-bt? n (node-left bt))]
    [else #false]))

; BT -> List-of-ssns
; Returns the sequence of all ssn numbers from a given binary tree
(check-expect (inorder b15) '(10 15 24))
(check-expect (inorder b29) '(10 15 24 29))
(check-expect (inorder b95) '(95 99))
(check-expect (inorder b89) '(77 89 95 99))
(check-expect (inorder b63) '(10 15 24 29 63 77 89 95 99))
(define (inorder a-btree)
  (cond
    [(and (no-info? (node-left a-btree))
          (no-info? (node-right a-btree))) (list (node-ssn a-btree))]
    [else (append
           (if (no-info? (node-left a-btree)) '()
               (inorder (node-left a-btree)))
           (list (node-ssn a-btree))
           (if (no-info? (node-right a-btree)) '()
             (inorder (node-right a-btree))))]
    ))
    
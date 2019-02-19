;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex336) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; A Dir.v1 (short for directory) is one of: 
; – '()
; – (cons File.v1 Dir.v1)
; – (cons Dir.v1 Dir.v1)
 
; A File.v1 is a String

(define tree.v1 (cons (cons (cons "part1" (cons "part2" (cons "part3" '()))) '())
                (cons "read!"
                (cons (cons (cons (cons "hang" (cons "draw" '())) '())
                      (cons (cons (cons "read!" '()) '()) '())) '()))))

; Dir.v1 -> Number
; Returns number of files
; in a given Dir.v1
(check-expect (how-many.v1 tree.v1) 7)
(define (how-many.v1 dir)
  (cond
    [(empty? dir) 0]
    [(string? dir) 1]
    [else
     (+ (how-many.v1 (first dir)) (how-many.v1 (rest dir)))]))

(define-struct dir [name content])

; A Dir.v2 is a structure: 
;   (make-dir String LOFD)
 
; An LOFD (short for list of files and directories) is one of:
; – '()
; – (cons File.v2 LOFD)
; – (cons Dir.v2 LOFD)
 
; A File.v2 is a String.

(define tree.v2
  (make-dir "TS"
    (cons (make-dir "Text"
      (cons "part1"
        (cons "part2"
          (cons "part3" '()))))
    (cons "read!"
    (cons (make-dir "Libs"
      (cons (make-dir "Code"
        (cons "hang" (cons "draw" '())))
      (cons (make-dir "Docs"
        (cons "read!" '())) '()))) '())))))

; Dir.v2 -> Number
; Returns the number of files in a given Dir.v2
(check-expect (how-many.v2 tree.v2) 7)
(define (how-many.v2 d)
  (cond
    [(empty? d) 0]
    [(string? d) 1]
    [(dir? d) (how-many.v2 (dir-content d))]
    [else (+ (how-many.v2 (first d))
             (how-many.v2 (rest d)))]))

(define-struct dir.v2 [name size readability content])

(define-struct file [name size content])
; A File.v3 is a structure: 
;   (make-file String N String)

(define-struct dir.v3 [name dirs files])
; A Dir.v3 is a structure: 
;   (make-dir.v3 String Dir* File*)
 
; A Dir* is one of: 
; – '()
; – (cons Dir.v3 Dir*)
 
; A File* is one of: 
; – '()
; – (cons File.v3 File*)

(define tree.v3
  (make-dir.v3 "TS"
               (list
                (make-dir.v3 "Text"
                             '()
                             (list
                              (make-file "part1" 99 "")
                              (make-file "part2" 52 "")
                              (make-file "part3" 17 "")))
                (make-dir.v3 "Libs"
                             (list
                              (make-dir.v3 "Code"
                                           '()
                                           (list
                                            (make-file "hang" 8 "")
                                            (make-file "draw" 2 "")))
                              (make-dir.v3 "Docs"
                                           '()
                                           (list
                                            (make-file "read!" 19 ""))))
                             '()))
               (list
                (make-file "read!" 10 ""))))

; Dir.v3 -> Number
; Returns the number of files in a given dir.v3
(check-expect (how-many.v3 tree.v3) 7)
(define (how-many.v3 d)
  (cond
    [(list? d)
     (+ (how-many.v3 (first (dir.v3-dirs d)))
        (how-many.v3 (rest (dir.v3-dirs d))))]
    [(empty? (dir.v3-dirs d)) 0]
    [else (+ (length (dir.v3-files d))
             (how-many.v3 (dir.v3-dirs d)))]))
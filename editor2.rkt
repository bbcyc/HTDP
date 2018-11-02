;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname editor2) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/universe)
(require 2htdp/image)

(define-struct editor [str pos])
; An Editor is a structure:
;   (make-editor String Number)
; interpretation (make-editor s t) describes an editor
; whose visible text is s with 
; the cursor displayed at the t position of s


; Editor -> Image
; Given an editor struct, render the image of the editor
(check-expect (render (make-editor "hello world" 6))
  (overlay/align "left" "center"
    (beside (rectangle 1 18 "solid" "black")
            (rectangle 1 18 "solid" "white")
            (text "hello " 16 "black")
            (rectangle 1 18 "solid" "red")
            (text "world" 16 "black"))
            (empty-scene 200 20)))
(define (render ed)
  (overlay/align "left" "center"
    (beside (rectangle 1 18 "solid" "black")
            (rectangle 1 18 "solid" "white")
            (text (pre ed) 16 "black")
            (rectangle 1 18 "solid" "red")
            (text (post ed) 16 "black"))
            (empty-scene 200 20)))

;Editor helper functions

; Editor -> Number
; Returns the width of the text for an editor
(define (text-width ed)
  (image-width
    (beside (rectangle 1 18 "solid" "black")
            (rectangle 1 18 "solid" "white")
            (text (pre ed) 16 "black")
            (rectangle 1 18 "solid" "red")
            (text (post ed) 16 "black"))))

; Editor -> String
; Given an editor, returns the string before the cursor
(check-expect (pre (make-editor "hello world" 6)) "hello ")
(define (pre ed)
  (substring (editor-str ed) 0 (editor-pos ed)))

; Editor -> String
; Given an editor, returns the string after the cursor
(check-expect (post (make-editor "hello world" 6)) "world")
(define (post ed)
  (substring (editor-str ed) (editor-pos ed)))

; String -> 1String
; returns the first 1String from a non-empty string. 
(check-expect (string-first "brian") "b")
(check-expect (string-first "") "")
(define (string-first str)
  (if (> (string-length str) 0)
    (substring str 0 1)
    ""))

; String -> 1String
; returns the last 1String from a non-empty string
(check-expect (string-last "brian") "n")
(check-expect (string-last "") "")
(define (string-last str)
  (if (> (string-length str) 0)
    (substring str (- (string-length str) 1))
    ""))

; String -> String
; returns a string with the first 1String removed
(check-expect (string-remove-first "brian") "rian")
(check-expect (string-remove-first "") "")
(define (string-remove-first str)
  (if (> (string-length str) 0)
    (substring str 1)
    ""))

; String -> String
; returns a string with the last 1String removed
(check-expect (string-remove-last "brian") "bria")
(check-expect (string-remove-last "") "")
(define (string-remove-last str)
  (if (> (string-length str) 0)
    (substring str 0 (- (string-length str) 1))
    ""))

; Editor KeyEvent -> Editor
; Takes an editor and a key event and returns a new editor
; "\b" - backspace
; "\t" "\r" - no effect
; all other chars append to pre
; "left" and "right" move cursor
(check-expect (edit (make-editor "hello world" 6) "left") (make-editor "hello world" 5))
(check-expect (edit (make-editor "hello world" 0) "left") (make-editor "hello world" 0))
(check-expect (edit (make-editor "hello world" 6) "right") (make-editor "hello world" 7))
(check-expect (edit (make-editor "hello world" 11) "right") (make-editor "hello world" 11))
(check-expect (edit (make-editor "hello world" 6) "up") (make-editor "hello world" 6))
(check-expect (edit (make-editor "hello world" 6) "down") (make-editor "hello world" 6))
(check-expect (edit (make-editor "hello world" 6) "\b") (make-editor "helloworld" 5))
(check-expect (edit (make-editor "hello world" 0) "\b") (make-editor "hello world" 0))
(check-expect (edit (make-editor "hello world" 6) "\t") (make-editor "hello world" 6))
(check-expect (edit (make-editor "hello world" 6) "\r") (make-editor "hello world" 6))
(check-expect (edit (make-editor "hello world" 6) "s") (make-editor "hello sworld" 7))
(check-expect (edit (make-editor "hello world" 0) "c") (make-editor "chello world" 1))
(check-expect (edit (make-editor "hello world" 11) "s") (make-editor "hello worlds" 12))
(define (edit ed ke)
  (cond
    [(key=? ke "left") (make-editor
                        (editor-str ed) (if (> (editor-pos ed) 0) (- (editor-pos ed) 1) 0))]
    [(key=? ke "right") (make-editor
                        (editor-str ed) (if (= (editor-pos ed) (string-length (editor-str ed)))
                                            (editor-pos ed) (+ (editor-pos ed) 1)))]
    [(> (string-length ke) 1) ed]
    [(and (key=? ke "\b") (> (editor-pos ed) 0)) (make-editor
                         (string-append (string-remove-last (pre ed))
                         (post ed))(- (editor-pos ed) 1))]
    [(and (key=? ke "\b") (<= (editor-pos ed) 0)) ed]
    [(or (key=? ke "\t") (key=? ke "\r")) ed]
    [(= (string-length ke) 1)
       (if (< (text-width (make-editor (string-append (pre ed) ke
                          (post ed)) (editor-pos ed))) 200)
           (make-editor (string-append (pre ed) ke
                          (post ed)) (+ (editor-pos ed) 1)) ed)]))
               

(define (run ed)
  (big-bang ed
    [on-key edit]
    [check-with editor?]
    [to-draw render]))
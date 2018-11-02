;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname editor) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/universe)
(require 2htdp/image)

(define-struct editor [pre post])
; An Editor is a structure:
;   (make-editor String String)
; interpretation (make-editor s t) describes an editor
; whose visible text is (string-append s t) with 
; the cursor displayed between s and t

; Editor -> Image
; Given an editor struct, render the image of the editor
(check-expect (render (make-editor "hello " "world"))
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
            (text (editor-pre ed) 16 "black")
            (rectangle 1 18 "solid" "red")
            (text (editor-post ed) 16 "black"))
            (empty-scene 200 20)))

;Editor helper functions

; Editor -> Number
; Returns the width of the text for an editor
(define (text-width ed)
  (image-width
    (beside (rectangle 1 18 "solid" "black")
            (rectangle 1 18 "solid" "white")
            (text (editor-pre ed) 16 "black")
            (rectangle 1 18 "solid" "red")
            (text (editor-post ed) 16 "black"))))

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
(check-expect (edit (make-editor "hello " "world") "left") (make-editor "hello" " world"))
(check-expect (edit (make-editor "" "hello world") "left") (make-editor "" "hello world"))
(check-expect (edit (make-editor "hello " "world") "right") (make-editor "hello w" "orld"))
(check-expect (edit (make-editor "hello world" "") "right") (make-editor "hello world" ""))
(check-expect (edit (make-editor "hello " "world") "up") (make-editor "hello " "world"))
(check-expect (edit (make-editor "hello " "world") "down") (make-editor "hello " "world"))
(check-expect (edit (make-editor "hello " "world") "\b") (make-editor "hello" "world"))
(check-expect (edit (make-editor "" "hello world") "\b") (make-editor "" "hello world"))
(check-expect (edit (make-editor "hello " "world") "\t") (make-editor "hello " "world"))
(check-expect (edit (make-editor "hello " "world") "\r") (make-editor "hello " "world"))
(check-expect (edit (make-editor "hello " "world") "s") (make-editor "hello s" "world"))
(check-expect (edit (make-editor "" "hello world") "c") (make-editor "c" "hello world"))
(check-expect (edit (make-editor "hello world" "") "s") (make-editor "hello worlds" ""))
(define (edit ed ke)
  (cond
    [(key=? ke "left") (make-editor
                        (string-remove-last (editor-pre ed))
                        (string-append (string-last (editor-pre ed))
                                       (editor-post ed)))]
    [(key=? ke "right") (make-editor
                         (string-append (editor-pre ed)
                                        (string-first (editor-post ed)))
                         (string-remove-first (editor-post ed)))]
    [(> (string-length ke) 1) ed]
    [(key=? ke "\b") (make-editor (string-remove-last (editor-pre ed))
                         (editor-post ed))]
    [(or (key=? ke "\t") (key=? ke "\r")) ed]
    [(= (string-length ke) 1)
       (if (< (text-width (make-editor (string-append (editor-pre ed) ke)
                          (editor-post ed))) 200)
           (make-editor (string-append (editor-pre ed) ke)
                          (editor-post ed)) ed)]))
               

(define (run ed)
  (big-bang ed
    [on-key edit]
    [to-draw render]))
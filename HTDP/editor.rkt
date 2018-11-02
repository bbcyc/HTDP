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
    [(key=? ke "\b") (BACKSPACE)]
    [(or (key=? ke "\t") (key=? ke "\r")) ed]
    [(= (string-length ke) 1) (ADD CHAR TO PRE STRING)]))
               

(render (make-editor "hello " "world"))
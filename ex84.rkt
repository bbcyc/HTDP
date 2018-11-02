;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ex84) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)

;Exercise 34. Design the function string-first, which extracts the first character from a non-empty string. Don’t worry about empty strings. 

;Exercise 35. Design the function string-last, which extracts the last character from a non-empty string. 

;Exercise 36. Design the function image-area, which counts the number of pixels in a given image. 

;Exercise 37. Design the function string-rest, which produces a string like the given one with the first character removed. 

;Exercise 38. Design the function string-remove-last, which produces a string like the given one with the last character removed. 

(define-struct editor [pre post])
; An Editor is a structure:
;   (make-editor String String)
; interpretation (make-editor s t) describes an editor
; whose visible text is (string-append s t) with 
; the cursor displayed between s and t

;(overlay/align "left" "center"
;               (beside
;                 (rectangle 1 10 "solid" "black")
;                 (rectangle 1 10 "solid" "white")
;                 (text "hello " 16 "black")
;                 (rectangle 1 20 "solid" "red")
;                 (text "world" 16 "black"))
;                 (empty-scene 200 20))

; Editor -> Image
; Takes an Editor and renders the empty scene,
; text, and cursor

(check-expect (render (make-editor "hello " "world"))
  (overlay/align "left" "center"
               (beside
                 (rectangle 1 10 "solid" "black")
                 (rectangle 1 10 "solid" "white")
                 (text "hello " 16 "black")
                 (rectangle 1 20 "solid" "red")
                 (text "world" 16 "black"))
               (empty-scene 200 20)))
(define (render editor)
  (overlay/align "left" "center"
               (beside
                 (rectangle 1 10 "solid" "black")
                 (rectangle 1 10 "solid" "white")
                 (text (editor-pre editor) 16 "black")
                 (rectangle 1 20 "solid" "red")
                 (text (editor-post editor) 16 "black"))
               (empty-scene 200 20)))

; Editor KeyEvent -> Editor
; Takes an editor and a key event and
; returns a new editor
; Key Event is an enumeration of:
; - a single letter
; - "left"
; - "right"
(define (edit ed ke)
  (if (= (string-length ke) 1)
    (cond
      [(key=? ke "\b") (REMOVE CHAR TO LEFT OF CURSOR)]
      [(or (key=? ke "\t") (key=? ke "\r")) ed]
      [else (ADD CHAR TO LEFT OF CURSOR)])
    (cond
      [(key=? ke "left") (MOVE CURSOR TO LEFT)]
      [(key=? ke "left") (MOVE CURSOR TO RIGHT)]
      [else ed])))
      
      



     ])
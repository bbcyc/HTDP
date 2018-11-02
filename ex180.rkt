;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ex180) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)

(define HEIGHT 20) ; the height of the editor 
(define WIDTH 200) ; its width 
(define FONT-SIZE 16) ; the font size 
(define FONT-COLOR "black") ; the font color 
 
(define MT (empty-scene WIDTH HEIGHT))
(define CURSOR (rectangle 1 HEIGHT "solid" "red"))



; Lo1s -> Lo1s 
; produces a reverse version of the given list 
 
(check-expect
  (rev (cons "a" (cons "b" (cons "c" '()))))
  (cons "c" (cons "b" (cons "a" '()))))
 
(define (rev l)
  (cond
    [(empty? l) '()]
    [else (add-at-end (rev (rest l)) (first l))]))

; Lo1s 1String -> Lo1s
; creates a new list by adding s to the end of l
(check-expect (add-at-end '() "s") (cons "s" '()))
(check-expect (add-at-end (cons "c" (cons "b" '())) "a")
              (cons "c" (cons "b" (cons "a" '()))))
(define (add-at-end l s)
  (cond
    [(empty? l) (cons s '())]
    [else (cons (first l) (add-at-end (rest l) s))]))

(define-struct editor [pre post])
; An Editor is a structure:
;   (make-editor Lo1S Lo1S) 
; An Lo1S is one of: 
; – '()
; – (cons 1String Lo1S)

(define good
  (cons "g" (cons "o" (cons "o" (cons "d" '())))))
(define all
  (cons "a" (cons "l" (cons "l" '()))))
(define lla
  (cons "l" (cons "l" (cons "a" '()))))

; (string-append (implode (rev pre))
;               (implode post))

(check-expect (create-editor "abc" "def")
              (make-editor (cons "c" (cons "b" (cons "a" '())))
                           (cons "d" (cons "e" (cons "f" '())))))
                                         
(define (create-editor s1 s2)
  (make-editor (rev (explode s1)) (explode s2)))

; Lo1s -> Image
; renders a list of 1Strings as a text image 
(check-expect (editor-text (cons "P" (cons "O" (cons "S" (cons "T" '())))))
              (text "POST" FONT-SIZE FONT-COLOR))
(define (editor-text s)
  (text (my-implode s) FONT-SIZE FONT-COLOR))

(define (my-implode l)
  (cond
    [(empty? l) ""]
    [else (string-append (first l) (my-implode (rest l)))]))

; Editor -> Image
; renders an editor as an image of the two texts 
; separated by the cursor 
(define (editor-render e)
  (place-image/align
  (beside (editor-text (reverse (editor-pre e)))
            CURSOR
            (editor-text (editor-post e)))
  1 1
  "left" "top"
  MT))

; Editor KeyEvent -> Editor
; deals with a key event, given some editor
(check-expect (editor-kh (create-editor "" "") "e")
              (create-editor "e" ""))
(check-expect
  (editor-kh (create-editor "cd" "fgh") "e")
  (create-editor "cde" "fgh"))
(check-expect (editor-kh (create-editor "" "") "e")
              (create-editor "e" ""))
(check-expect (editor-kh (create-editor "" "") "\b")
              (create-editor "" ""))
(check-expect (editor-kh (create-editor "abc" "def") "\b")
              (create-editor "ab" "def"))
(check-expect (editor-kh (create-editor "" "abc") "left")
              (create-editor "" "abc"))
(check-expect (editor-kh (create-editor "abc" "def") "left")
              (create-editor "ab" "cdef"))
(check-expect (editor-kh (create-editor "abc" "") "right")
              (create-editor "abc" ""))
(check-expect (editor-kh (create-editor "abc" "def") "up")
              (create-editor "abc" "def"))
(check-expect (editor-kh (create-editor "ab" "def") "c")
              (create-editor "abc" "def"))
(check-expect (editor-kh (create-editor "abc" "") "e")
              (create-editor "abce" ""))

(define (editor-kh ed k)
  (cond
    [(key=? k "left") (if (empty? (editor-pre ed)) ed
                          (make-editor (rest (editor-pre ed))
                                       (cons (first (editor-pre ed)) (editor-post ed))))]
    [(key=? k "right") (if (empty? (editor-post ed)) ed
                          (make-editor (cons (first (editor-post ed)) (editor-pre ed))
                                       (rest (editor-post ed))))]
    [(key=? k "\b") (if (empty? (editor-pre ed)) ed
                        (make-editor (rest (editor-pre ed)) (editor-post ed)))]
    [(key=? k "\t") (make-editor (cons " " (cons " " (cons " " (cons " " 
                                 (editor-pre ed))))) (editor-post ed))]
    [(key=? k "\r") ed]
    [(= (string-length k) 1) (make-editor (cons k (editor-pre ed)) (editor-post ed))]
    [else ed]))

; main : String -> Editor
; launches the editor given some initial string 
(define (main s)
   (big-bang (create-editor s "")
     [on-key editor-kh]
     [to-draw editor-render]))
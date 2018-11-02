;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ex109) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/universe)
(require 2htdp/image)

(define WIDTH 100)
(define HEIGHT 100)
(define XOFF (/ WIDTH 2))
(define YOFF (/ HEIGHT 2))

(define BACKGROUND
  (empty-scene WIDTH HEIGHT))


; A WorldState is one of 4 distinct states
(define AA "start, expect an 'a'")
(define BB "expect 'b', 'c', or 'd'")
(define DD "finished")
(define ER "error, illegal key")

; WorldState -> Image
; Takes a world state and returns an image of a colored rectangle
(define (render ws)
  (place-image
    (rectangle
      WIDTH
      HEIGHT
      "solid"
      (cond
        [(string=? ws AA) "white"]
        [(string=? ws BB) "yellow"]
        [(string=? ws DD) "green"]
        [(string=? ws ER) "red"]
        [else "blue"]))
      XOFF
      YOFF
      BACKGROUND))
    
; WorldState KeyEvent -> WorldState
(define (keystroke ws ke)
  (cond
    [(string=? ws AA) (if (key=? "a" ke) BB ER)]
    [(string=? ws BB)
      (cond
        [(key=? "b" ke) BB]
        [(key=? "c" ke) BB]
        [(key=? "d" ke) DD]
        [else ER])]
    [else ws]))

; WorldState is a Number representing the number
; of pixels between the left margin of the scene
; and the left margin of the cat
(define (ex109 ws)
  (big-bang ws
      [on-key keystroke]
      [to-draw render]))
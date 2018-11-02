;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ex215) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)

(define UP "up")
(define DOWN "down")
(define LEFT "left")
(define RIGHT "right")
(define SEGD 10)
(define SEGOS 5)
(define BACKGROUND (empty-scene (* SEGD 20) (* SEGD 20)))
(define SEG (circle (/ SEGD 2) "solid" "red"))

(define-struct worm [dir body])
; A Worm is a struct:
; (make-worm String List-of-Posn's)
; interpretation- the direction of motion
; and a list of posns representing
; the number of segments
; from the left and top of the box
 
; Worm -> Image
; when needed, big-bang obtains the image of the current 
; state of the world by evaluating (render cw)
(check-expect (render (make-worm UP (list (make-posn 0 0)))) (place-image SEG SEGOS SEGOS BACKGROUND)) 
(define (render w)
  (cond
    [(empty? w) BACKGROUND]
    [else (place-image SEG
                       (+ SEGOS (* SEGD (posn-x (first (worm-body w)))))
                       (+ SEGOS (* SEGD (posn-x (first (worm-body w)))))
                       (render (rest (worm-body w))))]))

; Direction Posn -> Posn
; Returns a new segment one SEGD from
; Posn in the direction DIR
(define (create-segment DIR POS)
  (cond
    [(string=? UP DIR) (make-posn (posn-x POS) (- (posn-y POS) SEGD))]
    [(string=? DOWN DIR) (make-posn (posn-x POS) (+ (posn-y POS) SEGD))]
    [(string=? LEFT DIR) (make-posn (- (posn-x POS) SEGD)  (posn-y POS))]
    [(string=? RIGHT DIR) (make-posn (+ (posn-x POS) SEGD) (posn-y POS))]))

(define (drop-last w)
  (cond
    [(empty? (rest w)) '()]
    [else (cons (first w) (drop-last (rest w)))]))

; WorldState -> WorldState
; for each tick of the clock, big-bang obtains the next 
; state of the world from (clock-tick-handler cw) 
(define (clock-tick-handler w)
  (make-worm (worm-dir w) (cons (create-segment (worm-dir w) (first (worm-body w))) (drop-last w))))
     
 
; WorldState String -> WorldState 
; for each keystroke, big-bang obtains the next state 
; from (keystroke-handler cw ke); ke represents the key
(define (keystroke-handler cw ke) ...)
 
; WorldState Number Number String -> WorldState 
; for each mouse gesture, big-bang obtains the next state
; from (mouse-event-handler cw x y me) where x and y are
; the coordinates of the event and me is its description 
(define (mouse-event-handler cw x y me) ...)
 
; WorldState -> Boolean
; after each event, big-bang evaluates (end? cw) 
(define (end? cw) ...)


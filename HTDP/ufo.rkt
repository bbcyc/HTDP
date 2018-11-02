;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ufo) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/universe)
(require 2htdp/image)

(define BG_WIDTH 300)
(define BG_HEIGHT (* BG_WIDTH 1.0))
(define GROUND
  (rectangle BG_WIDTH (* BG_HEIGHT .1) "solid" "brown"))
(define SKY
  (rectangle BG_WIDTH BG_HEIGHT "solid" "blue"))
(define BACKGROUND
  (place-images/align
     (list GROUND
           SKY)
     (list (make-posn 0 (* BG_HEIGHT 0.9))
           (make-posn 0 0))
     "left"
     "top"
     (empty-scene BG_WIDTH BG_HEIGHT)))
(define TANK
  (rectangle (* BG_WIDTH 0.1) (* BG_WIDTH 0.06) "solid" "black"))
(define TANK_HEIGHT (* BG_HEIGHT 0.87))
(define UFO
  (overlay (rectangle (* BG_WIDTH 0.1) (* BG_WIDTH 0.02) "solid" "green")
           (circle (* BG_WIDTH 0.02) "solid" "green")))
(define MISSILE
  (triangle (* BG_WIDTH 0.04) "solid" "black"))
(define INITIAL_SCENE
  (place-images
    (list UFO
          TANK
          MISSILE)
    (list (make-posn (* BG_WIDTH 0.5) (* BG_HEIGHT 0.1))
          (make-posn (* BG_WIDTH 0.2) (* BG_HEIGHT 0.87))
          (make-posn (* BG_WIDTH 0.2) (* BG_HEIGHT 0.87)))
    BACKGROUND))
(define GAME_LOST
  (place-image (text "You Lose!" 48 "black")
               (* BG_WIDTH 0.5)
               (* BG_HEIGHT 0.3)
               INITIAL_SCENE))
(define GAME_WON
  (place-image (text "You Win!" 48 "black")
               (* BG_WIDTH 0.5)
               (* BG_HEIGHT 0.3)
               INITIAL_SCENE))

(define-struct aim [ufo tank])
(define-struct fired [ufo tank missile])

; A UFO is a Posn. 
; interpretation (make-posn x y) is the UFO's location 
; (using the top-down, left-to-right convention)
 
(define-struct tank [loc vel])
; A Tank is a structure:
;   (make-tank Number Number). 
; interpretation (make-tank x dx) specifies the position:
; (x, HEIGHT) and the tank's speed: dx pixels/tick 
 
; A Missile is a Posn. 
; interpretation (make-posn x y) is the missile's place

; A SIGS is one of: 
; – (make-aim UFO Tank)
; – (make-fired UFO Tank Missile)
; interpretation represents the complete state of a 
; space invader game0
           
; WorldState -> WorldState
; Adds 3 to WorldState to make cat move from left to right
(define (tock ws)
  ws)

; Tank Image -> Image 
; adds t to the given image im
(check-expect (tank-render (make-tank 10 5) BACKGROUND)
  (place-image TANK 10 TANK_HEIGHT BACKGROUND))
(define (tank-render t im)
  (place-image TANK (tank-loc t) TANK_HEIGHT im))
 
; UFO Image -> Image 
; adds u to the given image im
(check-expect (ufo-render (make-posn 20 30) BACKGROUND)
  (place-image UFO 20 30 BACKGROUND))
(define (ufo-render u im)
  (place-image UFO (posn-x u) (posn-y u) im))

; Missile Image -> Image 
; adds m to the given image im
(check-expect (missile-render (make-posn 50 50) BACKGROUND)
  (place-image MISSILE 50 50 BACKGROUND))
(define (missile-render m im)
  (place-image MISSILE (posn-x m) (posn-y m) im))


; SIGS -> Image
; renders the given game state on top of BACKGROUND 
; for examples see figure 32
(define (si-render s)
  (cond
    [(aim? s)
     (tank-render (aim-tank s)
                  (ufo-render (aim-ufo s) BACKGROUND))]
    [(fired? s)
     (tank-render
       (fired-tank s)
       (ufo-render (fired-ufo s)
                   (missile-render (fired-missile s)
                                   BACKGROUND)))]))

; SIGS -> SIGS
; called every clock tick and returns a SIGS with the
; new position of each object
(define (si-move s)
  (cond
    [(aim? s) (make-aim (make-posn x y) (make-tank loc vel) s]
    [(fired? (make-fired (make-posn x y) (make-tank loc vel) (make-posn x y) s)



(define (keystroke ws ke)
  (cond
    [(key=? "space" ke) ws] ;fire missile
    [(key=? "left" ke) ws] ;change tank direction to left
    [(key=? "right" ke) ws] ;change tank direction to right
    [else ws]))

; Fired -> Boolean
; Takes a fired SIGS and returns #true if the missile struck the UFO,
; #false otherwise
(define (ufo-hit? s)
  (< (sqrt (+ (sqr (- (posn-x (fired-ufo s)) (posn-x (fired-missile s))))
    (sqr (- (posn-y (fired-ufo s)) (posn-y (fired-missile s))))))
    (* BG_WIDTH 0.05)))

; SIGS -> Boolean
; if UFO lands or missile hits the UFO, stop the program
(define (si-game-over? s)
  (cond
    [(aim? s) (>= (posn-y (fired-ufo s)) (* BG_HEIGHT 0.9))]
    [(fired? s) (or (ufo-hit? s) (>= (posn-y (fired-ufo s)) (* BG_HEIGHT 0.9)))]))
 
; SIGS -> Image
; Takes a SIGS and returns a "you won" final image if missile hit
; or a "you lost" image is the ufo lands
(define (si-render-final s)
  (cond
    [(aim? s) GAME_LOST]
    [(fired? s) (if (ufo-hit? s) GAME_WON GAME_LOST)]))


; WorldState is a Number representing the number
; of pixels between the left margin of the scene
; and the left margin of the cat
(define (cham-prog ws)
  (big-bang ws
      [on-tick tock]
      [on-key keystroke]
      [to-draw si-render]
      [stop-when si-game-over? si-render-final]))
;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ufov2) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
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

(define UFO_SPEED 1)
(define TANK_SPEED 3)
(define MISSILE_SPEED 2)

(define-struct sigs [ufo tank missile])
; A SIGS.v2 (short for SIGS version 2) is a structure:
;   (make-sigs UFO Tank MissileOrNot)
; interpretation represents the complete state of a
; space invader game
 
; A MissileOrNot is one of: 
; – #false
; – Posn
; interpretation#false means the missile is in the tank;
; Posn says the missile is at that location

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

(define (missile-or-not? v)
  (or (false? v) (posn? v)))

(define (SIGS-or-not? s)
  (and (posn? (sigs-ufo s))
       (tank? (sigs-tank s))
       (missile-or-not? (sigs-missile s))))

           
(define START
  (make-sigs
   (make-posn (* BG_WIDTH 0.5) (* BG_HEIGHT 0.1))
   (make-tank (* BG_WIDTH 0.1) TANK_SPEED)
   #false))

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

; MissileOrNot Image -> Image 
; adds an image of missile m to scene s
(check-expect (missile-render.v2 #false BACKGROUND) BACKGROUND)
(check-expect (missile-render.v2 (make-posn 40 40) BACKGROUND)
  (place-image MISSILE 40 40 BACKGROUND))    
(define (missile-render.v2 m s)
  (cond
    [(boolean? m) s]
    [(posn? m)
     (place-image MISSILE (posn-x m) (posn-y m) s)]))

; SIGS.v2 -> Image 
; renders the given game state on top of BACKGROUND 
(define (si-render.v2 s)
  (tank-render
    (sigs-tank s)
    (ufo-render (sigs-ufo s)
                (missile-render.v2 (sigs-missile s)
                                   BACKGROUND))))


; SIGS -> SIGS
; called every clock tick and returns a SIGS with the
; new position of each object
(define (si-move-proper s delta)
  (make-sigs
    (make-posn (+ (posn-x (sigs-ufo s)) delta)
               (+ (posn-y (sigs-ufo s)) UFO_SPEED))
    (make-tank (+ (tank-loc (sigs-tank s)) (tank-vel (sigs-tank s)))
               (tank-vel (sigs-tank s)))
    (cond
      [(boolean? (sigs-missile s)) #false]
      [(posn? (sigs-missile s))
         (make-posn (posn-x (sigs-missile s))
                    (- (posn-y (sigs-missile s)) MISSILE_SPEED))])))

(define (si-move w)
  (si-move-proper w (- 5 (random 11))))    

; SIGS KeyEvent -> SIGS
; takes a game state and key event and returns a game state
; "left" arrow moves tank left
; "right" arrow moves tank right
; "space" bar launches missile
(define (si-control s ke)
  (cond
    [(string=? " " ke)
       (if (boolean? (sigs-missile s))
           (make-sigs
             (make-posn (posn-x (sigs-ufo s)) (posn-y (sigs-ufo s)))
             (make-tank (tank-loc (sigs-tank s)) (tank-vel (sigs-tank s)))
             (make-posn (tank-loc (sigs-tank s)) TANK_HEIGHT))
           s)] 
    [(string=? "left" ke)
      (make-sigs
        (make-posn (posn-x (sigs-ufo s)) (posn-y (sigs-ufo s)))
        (make-tank (tank-loc (sigs-tank s))
          (if (> (tank-vel (sigs-tank s)) 0)
              (* (tank-vel (sigs-tank s)) -1)
              (tank-vel (sigs-tank s))))
        (cond
          [(boolean? (sigs-missile s)) #false]
          [(posn? (sigs-missile s))
            (make-posn (posn-x (sigs-missile s))
                       (posn-y (sigs-missile s)))]))]
    [(string=? "right" ke)
     (make-sigs
        (make-posn (posn-x (sigs-ufo s)) (posn-y (sigs-ufo s)))
        (make-tank (tank-loc (sigs-tank s))
          (if (< (tank-vel (sigs-tank s)) 0)
              (* (tank-vel (sigs-tank s)) -1)
              (tank-vel (sigs-tank s))))
        (cond
          [(boolean? (sigs-missile s)) #false]
          [(posn? (sigs-missile s))
            (make-posn (posn-x (sigs-missile s))
                       (posn-y (sigs-missile s)))]))]
    [else s]))

; SIGS -> Boolean
; Takes a SIGS and returns #true if the missile struck the UFO,
; #false otherwise
(define (ufo-hit? s)
  (cond
    [(boolean? (sigs-missile s)) #false]
    [(posn? (sigs-missile s))
      (< (sqrt (+ (sqr (- (posn-x (sigs-ufo s)) (posn-x (sigs-missile s))))
        (sqr (- (posn-y (sigs-ufo s)) (posn-y (sigs-missile s))))))
        (* BG_WIDTH 0.05))]))

; SIGS -> Boolean
; if UFO lands or missile hits the UFO, stop the program
(define (si-game-over? s)
  (or (ufo-hit? s) (>= (posn-y (sigs-ufo s)) (* BG_HEIGHT 0.9))))
 
; SIGS -> Image
; Takes a SIGS and returns a "you won" final image if missile hit
; or a "you lost" image is the ufo lands
(define (si-render-final s)
  (if (ufo-hit? s) GAME_WON GAME_LOST))


; WorldState is a Number representing the number
; of pixels between the left margin of the scene
; and the left margin of the cat
(define (si-main ws)
  (big-bang ws
      [on-tick si-move]
      [on-key si-control]
      [to-draw si-render.v2]
      [check-with SIGS-or-not?] 
      [stop-when si-game-over? si-render-final]))
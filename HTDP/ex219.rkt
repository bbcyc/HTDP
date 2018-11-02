;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ex219) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)

(define UP "up")
(define DOWN "down")
(define LEFT "left")
(define RIGHT "right")
(define SEGD 10)
(define SEGOS 5)
(define SEGWIDTH 20)
(define SEGHEIGHT 20)
(define BACKGROUND (empty-scene (* SEGD SEGWIDTH) (* SEGD SEGHEIGHT)))
(define SEG (circle (/ SEGD 2) "solid" "red"))
(define FOOD (circle (/ SEGD 2) "solid" "green"))
; Posn -> Posn 
; ???
(check-satisfied (food-create (make-posn 1 1)) not=-1-1?)
(define (food-create p)
  (food-check-create
     p (make-posn (random SEGWIDTH) (random SEGHEIGHT))))
 
; Posn Posn -> Posn 
; generative recursion 
; ???
(define (food-check-create p candidate)
  (if (equal? p candidate) (food-create p) candidate))
 
; Posn -> Boolean
; use for testing only 
(define (not=-1-1? p)
  (not (and (= (posn-x p) 1) (= (posn-y p) 1))))

(define-struct worm [dir body food])
; A Worm is a struct:
; (make-worm String List-of-Posn's Posn)
; interpretation- the direction of motion,
; a list of posns representing
; the number of segments
; from the left and top of the box,
; and the location of food

(define WORM1 (make-worm UP (list (make-posn 10 10)) (make-posn 5 5))) 
(define WORM2 (make-worm UP (list (make-posn 10 10)) (make-posn 5 5)))
(define WORM3 (make-worm UP (list (make-posn 10 10) (make-posn 11 10) (make-posn 12 10)) (make-posn 5 5)))
(define WORM5 (make-worm UP (list (make-posn 10 10) (make-posn 11 10) (make-posn 12 10)
                                  (make-posn 12 11) (make-posn 11 11)) (make-posn 5 5)))

; Worm -> Image
; when needed, big-bang obtains the image of the current 
; state of the world by evaluating (render cw)
(check-expect (render (make-worm UP (list (make-posn 0 0)) (make-posn 5 5)))
              (place-image SEG SEGOS SEGOS
                           (place-image FOOD (+ SEGOS (* SEGD 5)) (+ SEGOS (* SEGD 5)) BACKGROUND)))
(define (render w)
  (cond
    [(empty? (worm-body w)) (place-image FOOD
                                         (+ SEGOS (* SEGD (posn-x (worm-food w))))
                                         (+ SEGOS (* SEGD (posn-y (worm-food w))))
                                         BACKGROUND)]
    [else (place-image SEG
                       (+ SEGOS (* SEGD (posn-x (first (worm-body w)))))
                       (+ SEGOS (* SEGD (posn-y (first (worm-body w)))))
                       (render (make-worm (worm-dir w) (rest (worm-body w)) (worm-food w))))]))


; Direction Posn -> Posn
; Returns a new segment one SEGD from
; Posn in the direction DIR
(check-expect (create-segment UP (make-posn 2 2)) (make-posn 2 1))
(check-expect (create-segment DOWN (make-posn 2 2)) (make-posn 2 3))
(check-expect (create-segment LEFT (make-posn 2 2)) (make-posn 1 2))
(check-expect (create-segment RIGHT (make-posn 2 2)) (make-posn 3 2))
(define (create-segment DIR POS)
  (cond
    [(string=? UP DIR) (make-posn (posn-x POS) (- (posn-y POS) 1))]
    [(string=? DOWN DIR) (make-posn (posn-x POS) (+ (posn-y POS) 1))]
    [(string=? LEFT DIR) (make-posn (- (posn-x POS) 1)  (posn-y POS))]
    [(string=? RIGHT DIR) (make-posn (+ (posn-x POS) 1) (posn-y POS))]))

; List-of-Posns -> List-of-Posns
; Removes the last member from a
; list of posns
(check-expect (drop-last (worm-body WORM1)) '())
(define (drop-last w)
  (cond
    [(empty? (rest w)) '()]
    [else (cons (first w) (drop-last (rest w)))]))

; WorldState -> WorldState
; for each tick of the clock, big-bang obtains the next 
; state of the world from (clock-tick-handler cw) 
(define (clock-tick-handler w)
  (if (and (= (posn-x (worm-food w))
              (posn-x (create-segment (worm-dir w) (first (worm-body w)))))
           (= (posn-y (worm-food w))
              (posn-y (create-segment (worm-dir w) (first (worm-body w))))))
     (make-worm (worm-dir w)
             (cons (create-segment (worm-dir w) (first (worm-body w)))
                   (worm-body w)) (food-create (make-posn SEGWIDTH SEGHEIGHT)))
     (make-worm (worm-dir w)
             (cons (create-segment (worm-dir w) (first (worm-body w)))
                   (drop-last (worm-body w))) (worm-food w))))
     
 
; WorldState String -> WorldState 
; for each keystroke, big-bang obtains the next state 
; from (keystroke-handler cw ke); ke represents the key
(define (keystroke-handler w ke)
  (cond
    [(key=? ke UP) (make-worm UP (worm-body w) (worm-food w))]
    [(key=? ke DOWN) (make-worm DOWN (worm-body w) (worm-food w))]
    [(key=? ke LEFT) (make-worm LEFT (worm-body w) (worm-food w))]
    [(key=? ke RIGHT) (make-worm RIGHT (worm-body w) (worm-food w))]
    [else w]))

; WorldState -> Boolean
; Given a worm, return true if it
; has hit itself
(define (hit-itself? w)
  (if (member? (first (worm-body w)) (rest (worm-body w))) #true #false))

; WorldState -> Boolean
; after each event, big-bang evaluates (end? cw) 
(define (end? w)
  (cond
    [(or (> 0 (posn-y (first (worm-body w))))
         (> 0 (posn-x (first (worm-body w))))
         (< SEGWIDTH (posn-x (first (worm-body w))))
         (< SEGHEIGHT (posn-y (first (worm-body w))))
         (hit-itself? w)) #true]
    [else #false]))

(define (final-scene w)
  (place-image (text (if (hit-itself? w)
                         "worm ran into itself"
                         "worm hit border") 16 "red")
               (/ (* SEGD 4 SEGWIDTH) 10)
               (/ (* SEGD 9 SEGHEIGHT) 10)
               (render w)))


; Worm -> Worm
(define (worm-main w)
  (big-bang w
    (on-tick clock-tick-handler 1)
    (on-key keystroke-handler)
    (to-draw render)
    (stop-when end? final-scene)))

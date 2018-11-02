;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ex220) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)

(define WIDTH 10) ; # of blocks, horizontally
(define HEIGHT 10) ; # of blocks, vertically
(define SIZE 10) ; blocks are squares
(define SCENE-SIZE (* WIDTH SIZE))
(define OFFSET 5)
 
 
(define BLOCK ; red squares with black rims
  (overlay
    (square (- SIZE 1) "solid" "red")
    (square SIZE "outline" "black")))

(define-struct tetris [block landscape])
(define-struct block [x y])
 
; A Tetris is a structure:
;   (make-tetris Block Landscape)
; A Landscape is one of: 
; – '() 
; – (cons Block Landscape)
; A Block is a structure:
;   (make-block N N)
 
; interpretations
; (make-block x y) depicts a block whose left 
; corner is (* x SIZE) pixels from the left and
; (* y SIZE) pixels from the top;
; (make-tetris b0 (list b1 b2 ...)) means b0 is the
; dropping block, while b1, b2, and ... are resting

(define landscape0 (empty-scene SCENE-SIZE SCENE-SIZE))
(define block-top (make-block 0 0))
(define block-top1 (make-block 1 0))
(define block-landed9 (make-block (- WIDTH 1) (- HEIGHT 1)))
(define block-dropping (make-block 0 1))
(define tetris0 (make-tetris block-top '()))
(define tetris0-drop (make-tetris block-dropping '()))
(define block-landed (make-block 0 (- HEIGHT 1)))
(define block-on-block (make-block 0 (- HEIGHT 2)))
(define tetris2 (make-tetris block-top (list block-on-block block-landed)))
(define tetris3 (make-tetris block-on-block (list block-landed)))
(define tetris4 (make-tetris block-dropping (list block-on-block block-landed)))
(define tetris5 (make-tetris block-top1 (list block-on-block block-landed)))

(check-expect (tetris-render tetris0) (place-image BLOCK
                                                   (+ OFFSET (block-x block-top))
                                                   (+ OFFSET (block-y block-top))
                                                   landscape0))
(check-expect (tetris-render tetris2)
              (place-image BLOCK
                           (+ OFFSET (* (block-x block-on-block) SIZE))
                           (+ OFFSET (* (block-y block-on-block) SIZE))
                           (place-image BLOCK
                                        (+ OFFSET (* (block-x block-landed) SIZE))
                                        (+ OFFSET (* (block-y block-landed) SIZE))
                                        (place-image BLOCK
                                                     (+ OFFSET (* (block-x block-top) SIZE))
                                                     (+ OFFSET (* (block-y block-top) SIZE))
                                                     landscape0))))
(define (tetris-render t)
  (cond
    [(empty? (tetris-landscape t)) (place-image BLOCK
                                                (+ OFFSET (block-x (tetris-block t)))
                                                (+ OFFSET (block-y (tetris-block t)))
                                                landscape0)]
    [else (place-image BLOCK
                       (+ OFFSET (* (block-x (first (tetris-landscape t))) SIZE))
                       (+ OFFSET (* (block-y (first (tetris-landscape t))) SIZE))
                       (tetris-render (make-tetris (tetris-block t)
                                                   (rest (tetris-landscape t)))))]))

; Tetris -> Boolean
; Returns true if block is
; resting on another block
; or resting on bottom
(check-expect (touching? tetris2) #false)
(check-expect (touching? tetris3) #true)
(define (touching? t)
  (if
    (or (member? (make-block (block-x (tetris-block t)) (+ 1 (block-y (tetris-block t))))
              (tetris-landscape t))
        (= (- HEIGHT 1) (block-y (tetris-block t)))) #true #false))
    
; Block -> Block
; returns a new block
; one step to the right
; of the old
(check-expect (create-block block-top) block-top1)
(check-expect (create-block block-landed9) block-top)
(define (create-block b)
  (if (< (block-x b) (- WIDTH 1))
      (make-block (+ 1 (block-x b)) 0)
      (make-block 0 0)))


; Tetris -> Tetris
; Drops block one space per clock tick
; until it touches bottom or another block
; then creates a new block one space to the
; right of the old one
(check-expect (tock tetris2) tetris4)
(check-expect (tock tetris3) tetris5)
(define (tock t)
  (cond
    [(touching? t) (make-tetris (create-block (tetris-block t))
                                (cons (tetris-block t) (tetris-landscape t)))]
    [else (make-tetris (make-block (block-x (tetris-block t))
                                   (+ 1 (block-y (tetris-block t))))
                       (tetris-landscape t))]))
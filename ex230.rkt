;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname ex230) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)

(define-struct fsm [initial transitions final])
(define-struct transition [current key next])
; An FSM.v2 is a structure: 
;   (make-fsm FSM-State LOT FSM-State)
; A LOT is one of: 
; – '() 
; – (cons Transition.v3 LOT)
; A Transition.v3 is a structure: 
;   (make-transition FSM-State KeyEvent FSM-State)

(define abd-fsm
  (list (make-transition "white" "a" "yellow")
        (make-transition "yellow" "b" "yellow")
        (make-transition "yellow" "c" "yellow")
        (make-transition "yellow" "d" "green")))

(define abd0 (make-fsm "white" abd-fsm "green"))

; FSM-State is a Color.
 
; interpretation An FSM represents the transitions that a
; finite state machine can take from one state to another 
; in reaction to keystrokes

; FSM FSM-State -> SimulationState.v2 
; match the keys pressed with the given FSM 
(define (simulate.v2 an-fsm)
  (big-bang an-fsm
    [to-draw state-as-colored-square]
    [on-key find-next-state]
    [stop-when end?]))

; SimulationState.v2 -> Image 
; renders current world state as a colored square 
 
(check-expect (state-as-colored-square abd0)
              (square 100 "solid" "white"))
 
(define (state-as-colored-square an-fsm)
  (square 100 "solid" (fsm-initial an-fsm)))

(define (find-next-state an-fsm ke)
  (make-fsm (find (fsm-transitions an-fsm) ke (fsm-initial an-fsm))
            (fsm-transitions an-fsm) (fsm-final an-fsm)))

; FSM KeyEvent FSM-State -> FSM-State
; finds the state representing current in transitions
; and retrieves the next field 

(define (find transitions ke current)
  (cond
    [(empty? transitions) (error "not found: " current)]
    [else (if (and (string=? (transition-current (first transitions)) current)
                   (string=? (transition-key (first transitions)) ke))
              (transition-next (first transitions))
              (find (rest transitions) ke current))]))

(define (end? an-fsm)
  (if (string=? (fsm-initial an-fsm) (fsm-final an-fsm))
      #true #false))
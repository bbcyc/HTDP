;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ex226) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)

; An FSM is one of:
;   – '()
;   – (cons Transition FSM)
 
(define-struct transition [current next])
; A Transition is a structure:
;   (make-transition FSM-State FSM-State)
 
; FSM-State is a Color.
 
; interpretation An FSM represents the transitions that a
; finite state machine can take from one state to another 
; in reaction to keystrokes

(define fsm-traffic
  (list (make-transition "red" "green")
        (make-transition "green" "yellow")
        (make-transition "yellow" "red")))
(define trans1 (make-transition "red" "greem"))
(define trans2 (make-transition "green" "yellow"))
(define trans3 (make-transition "yellow" "red"))

; Transition Transition -> Boolean
; Returns #true if two transitions
; are equal
(check-expect (state=? trans1 trans1) #true)
(check-expect (state=? trans2 trans1) #false)
(define (state=? t1 t2)
  (if (and (string=? (transition-current t1) (transition-current t2))
           (string=? (transition-next t1) (transition-next t2)))
      #true #false))
  
  
;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex328) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(check-expect (atom? 4) #true)
(check-expect (atom? "a") #true)
(check-expect (atom? 'sym) #true)
(check-expect (atom? '(1 2 3)) #false)
(define (atom? d)
  (or (number? d)
      (string? d)
      (symbol? d)))

; S-expr Symbol Atom -> S-expr
; replaces all occurrences of old in sexp with new

(check-expect (substitute '(((world) bye) bye) 'bye '42)
              '(((world) 42) 42))
(check-expect (substitute '((("world") 7) bye) 'bye '42)
              '((("world") 7) 42))
(check-expect (substitute '(hello 20.12 "world" bye) 'bye '42)
              '(hello 20.12 "world" 42))
(check-expect (substitute '((hello 20.12 "world" bye)) 'bye '42)
              '((hello 20.12 "world" 42)))
(define (substitute sexp old new)
  (local (; S-expr -> S-expr
          (define (for-sexp sexp)
            (cond
              [(atom? sexp) (for-atom sexp)]
              [else (for-sl sexp)]))
          ; SL -> S-expr 
          (define (for-sl sl)
            (cond
              [(empty? sl) '()]
              [else (cons (for-sexp (first sl))
                          (for-sl (rest sl)))]))
          ; Atom -> S-expr
          (define (for-atom at)
            (cond
              [(number? at) at]
              [(string? at) at]
              [(symbol? at) (if (equal? at old) new at)])))
    (for-sexp sexp)))
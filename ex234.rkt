;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname ex234) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(check-expect '(1 "a" 2 #false 3 "c")
              (list 1 "a" 2 #false 3 "c"))

(check-expect '() (list ))


(check-expect '(("alan" 1000)
                ("barb" 2000)
                ("carl" 1500))
              (list (list "alan" 1000)
                    (list "barb" 2000)
                    (list "carl" 1500)))

; String String -> ... deeply nested list ...
; produces a web page with given author and title
(define (my-first-web-page author title)
  `(html
     (head
       (title ,title)
       (meta ((http-equiv "content-type")
              (content "text-html"))))
     (body
       (h1 ,title)
       (p "I, " ,author ", made this page."))))

(check-expect `(1 "a" 2 #false 3 "c")
              (list 1 "a" 2 #false 3 "c"))

(check-expect `(("alan" ,(* 2 500))
                ("barb" 2000)
                (,(string-append "carl" " , the great") 1500)
                ("dawn" 2300))
              (list (list "alan" 1000)
                    (list "barb" 2000)
                    (list "carl , the great" 1500)
                    (list "dawn" 2300)))


(define title "ratings")

(check-expect `(html
   (head
     (title ,title))
   (body
     (h1 ,title)
     (p "A second web page")))
              (list 'html
                    (list 'head
                          (list 'title "ratings"))
                    (list 'body
                          (list 'h1 "ratings")
                          (list 'p "A second web page"))))

; List-of-numbers -> ... nested list ...
; creates a row for an HTML table from l
(define (make-row l)
  (cond
    [(empty? l) '()]
    [else (cons (make-cell (first l))
                (make-row (rest l)))]))
 
; Number -> ... nested list ...
; creates a cell for an HTML table from a number 
(define (make-cell n)
  `(td ,(number->string n)))

; List-of-numbers List-of-numbers -> ... nested list ...
; creates an HTML table from two lists of numbers 
(define (make-table row1 row2)
  `(table ((border "1"))
          (tr ,@(make-row row1))
          (tr ,@(make-row row2))))

(check-expect `(0 ,@'(1 2 3) 4) (list 0 1 2 3 4))

(check-expect `(("alan" ,(* 2 500))
                ("barb" 2000)
                (,@'("carl" " , the great")   1500)
                ("dawn" 2300))
              (list (list "alan" 1000)
                    (list "barb" 2000)
                    (list "carl" " , the great" 1500)
                    (list "dawn" 2300)))

(check-expect `(html
                (body
                 (table ((border "1"))
                        (tr ((width "200"))
                            ,@(make-row '( 1  2)))
                        (tr ((width "200"))
                            ,@(make-row '(99 65))))))
              (list 'html
                    (list 'body
                          (list 'table
                                (list (list 'border "1"))
                                (list 'tr (list (list 'width "200"))
                                      (list 'td "1")
                                      (list 'td "2"))
                                (list 'tr (list (list 'width "200"))
                                      (list 'td "99")
                                      (list 'td "65"))))))


(define one-list
  '("Asia: Heat of the Moment"
    "U2: One"
    "The White Stripes: Seven Nation Army"))

; LOS -> List of list of ranks and strings
; Returns a list of list of ranks and strings for a
; given list
(define (ranking los)
  (reverse (add-ranks (reverse los))))

(define (make-ranking-table lor)
  (cond
    [(empty? lor) '()]
    [else (cons (make-ranking-row (first lor))
                (make-ranking-table (rest lor)))]))

(define (make-ranking-row r)
  `(tr ((width "200"))
       (td ,(number->string (first r)))
       (td ,(second r))))
 
; LOS -> List of list of ranks and strings
; returns a list of list of lists with numbers and strings
(define (add-ranks los)
  (cond
    [(empty? los) '()]
    [else (cons (list (length los) (first los))
                (add-ranks (rest los)))]))
(define rankings "Top 3 Songs from the 80s, 90s, 00s")
(define (make-ranking los)
   `(html
     (head
      (title ,rankings))
     (body
      (table ((border "1"))
        ,@(make-ranking-table (ranking los))))))
       
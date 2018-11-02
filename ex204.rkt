;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname ex204) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/batch-io)
(require 2htdp/itunes)

; the 2htdp/itunes library documentation, part 1: 
 
; An LTracks is one of:
; – '()
; – (cons Track LTracks)
 
;(define-struct track
;  [name artist album time track# added play# played])
; A Track is a structure:
;   (make-track String String String N N Date N Date)
; interpretation An instance records in order: the track's 
; title, its producing artist, to which album it belongs, 
; its playing time in milliseconds, its position within the 
; album, the date it was added, how often it has been 
; played, and the date when it was last played
 
;(define-struct date [year month day hour minute second])
; A Date is a structure:
;   (make-date N N N N N N)
; interpretation An instance records six pieces of information:
; the date's year, month (between 1 and 12 inclusive), 
; day (between 1 and 31), hour (between 0 
; and 23), minute (between 0 and 59), and 
; second (also between 0 and 59).

; Any Any Any Any Any Any Any Any -> Track or #false
; creates an instance of Track for legitimate inputs
; otherwise it produces #false
;(define (create-track name artist album time
;                      track# added play# played)
;  ...)
 
; Any Any Any Any Any Any -> Date or #false
; creates an instance of Date for legitimate inputs 
; otherwise it produces #false
;(define (create-date y mo day h m s)
;  ...)
 
; String -> LTracks
; creates a list-of-tracks representation from the
; text in file-name (an XML export from iTunes)
;(define (read-itunes-as-tracks file-name)
;  ...)

; modify the following to use your chosen name
; (define ITUNES-LOCATION "itunes.xml")
 
; LTracks
;(define itunes-tracks
;  (read-itunes-as-tracks ITUNES-LOCATION))

(define DATE1 (create-date 2018 04 21 5 56 34))
(define DATE2 (create-date 1983 02 22 14 43 35))
(define DATE3 (create-date 2011 09 28 12 23 45))
(define DATE4 (create-date 2004 04 11 22 08 44))
(define DATE5 (create-date 2004 04 11 22 08 45))
;(create-track name artist album time
;                      track# added play# played)
(define TRACK1 (create-track "tr01" "ar01" "al01" 22344 1 DATE4 12 DATE1))
(define TRACK2 (create-track "tr02" "ar01" "al01" 32344 2 DATE4 1 DATE2))
(define TRACK3 (create-track "tr03" "ar01" "al01" 27644 3 DATE4 2 DATE3))
(define TRACK4 (create-track "tr01" "ar02" "al02" 81233 1 DATE4 32 DATE4))
(define TRACK5 (create-track "tr02" "ar02" "al02" 22323 2 DATE3 22 DATE1))
(define TRACK6 (create-track "tr03" "ar02" "al02" 46765 3 DATE2 19 DATE2))
(define TRACK7 (create-track "tr01" "ar03" "al03" 11145 1 DATE1 33 DATE3))
(define TRACK8 (create-track "tr02" "ar03" "al03" 24684 2 DATE3 44 DATE4))
(define TRACK9 (create-track "tr01" "ar01" "al04" 22345 1 DATE2 15 DATE1))
(define TRACK0 (create-track "tr02" "ar01" "al04" 73532 2 DATE2 73 DATE1))

(define LTRACK1 (list TRACK1 TRACK2 TRACK3 TRACK4 TRACK5 TRACK6 TRACK7 TRACK8
                      TRACK9 TRACK0))
(define LTRACK2 (list TRACK1 TRACK2 TRACK3))
(define LTRACK3 (list TRACK4 TRACK5 TRACK6))
(define LTRACK4 (list TRACK7 TRACK8))
(define LTRACK5 (list TRACK9 TRACK0))

(define LTRACK6 (list TRACK4 TRACK1 TRACK3 TRACK0 TRACK5 TRACK7))


; LTracks -> Number
; returns the total play time for a given LTracks
(check-expect (total-time LTRACK2) 82332) 
(define (total-time l)
  (cond
    [(empty? l) 0]
    [else (+ (track-time (first l)) (total-time (rest l)))]))

; LTracks -> List-of-Strings
; returns all album titles from a given LTracks
(check-expect (select-all-album-titles LTRACK2) (list "al01" "al01" "al01"))
(define (select-all-album-titles l)
  (cond
    [(empty? l) '()]
    [else (cons (track-album (first l)) (select-all-album-titles (rest l)))]))

; List-of-Strings -> List-of-Strings
; Dedupes a list of strings
(check-expect (create-set (list "b" "b" "c" "d" "c" "b" "a")) (list "d" "c" "b" "a"))
(define (create-set los)
  (cond
    [(empty? los) '()]
    [(empty? (rest los)) los]
    [(is-dupe? (first los) (rest los)) (create-set (rest los))]
    [else (cons (first los) (create-set (rest los)))]))

; String List-of-Strings -> Boolean
; Returns #true is String is in List-of-Strings
(check-expect (is-dupe? "brian" '()) #false)
(check-expect (is-dupe? "brian" (list "david" "brian")) #true)
(check-expect (is-dupe? "brian" (list "kyle" "chrissy")) #false)

(define (is-dupe? s los)
  (cond
    [(empty? los) #false]
    [(string=? s (first los)) #true]
    [else (is-dupe? s (rest los))]))

; LTracks -> List-of-Strings
; returns a list of unique album titles from a given LTracks
(check-expect (select-album-titles/unique LTRACK1) (list "al01" "al02" "al03" "al04"))
(define (select-album-titles/unique l)
  (create-set (select-all-album-titles l)))

; String LTracks -> LTracks
; returns an LTracks containing the tracks with a given album name
(check-expect (select-album "al02" LTRACK1) LTRACK3)
(define (select-album s l)
  (cond
    [(empty? l) '()]
    [(string=? s (track-album (first l)))
     (cons (first l) (select-album s (rest l)))]
    [else (select-album s (rest l))]))

; String Date LTracks -> LTracks
; returns a list-of-tracks from the given LTracks
; that have the title String and
; were played after Date
(check-expect (select-album-date "al01" (create-date 2000 01 01 01 01 01) LTRACK1)
              (list TRACK1 TRACK3))
(define (select-album-date s d l)
  (cond
    [(empty? l) '()]
    [(and (string=? s (track-album (first l)))
          (date<? d (track-played (first l))))
     (cons (first l) (select-album-date s d (rest l)))]
    [else (select-album-date s d (rest l))]))
; (define-struct date [year month day hour minute second])
; Date Date -> Boolean
; returns true is the first date is before the second
(check-expect (date<? DATE1 DATE2) #false)
(check-expect (date<? DATE2 DATE3) #true)
(check-expect (date<? DATE4 DATE5) #true)
(define (date<? d1 d2)
  (if (not (= (date-year d1) (date-year d2)))
      (< (date-year d1) (date-year d2))
      (if (not (= (date-month d1) (date-month d2)))
          (< (date-month d1) (date-month d2))
          (if (not (= (date-day d1) (date-day d2)))
              (< (date-day d1) (date-day d2))
              (if (not (= (date-hour d1) (date-hour d2)))
                  (< (date-hour d1) (date-hour d2))
                  (if (not (= (date-minute d1) (date-minute d2)))
                      (< (date-minute d1) (date-minute d2))
                      (if (not (= (date-second d1) (date-second d2)))
                          (< (date-second d1) (date-second d2)) #false)))))))

; LTracks -> List-of-LTracks
; returns of list of LTracks, one for each album
(check-expect (select-albums LTRACK1) (list LTRACK2 LTRACK3 LTRACK4 LTRACK5))
(define (select-albums l)
  (cond
    [(empty? l) '()]
    [else (cons
           (create-set-ltrack
                 (get-all-for-album
                  (first (select-album-titles/unique l)) l))
                (select-albums (rest (select-album-titles/unique l))))])) ; this is wrong

; Title LTracks -> LTracks
; Given an album title, returns an LTracks for that album
(check-expect (get-all-for-album "al01" LTRACK1) LTRACK2)
(define (get-all-for-album t l)
  (cond
    [(empty? l) '()]
    [(string=? t (track-album (first l)))
     (cons (first l) (get-all-for-album t (rest l)))]
    [else (get-all-for-album t (rest l))]))

; LTracks -> Ltracks
; Returns a deduped LTracks for one album
; from a given LTracks for one album
(check-expect (create-set-ltrack (list TRACK3 TRACK2 TRACK1 TRACK1 TRACK2 TRACK3))
              LTRACK2) 
(define (create-set-ltrack l)
  (cond
    [(empty? l) '()]
    [(empty? (rest l)) l]
    [(is-dupe-track? (first l) (rest l)) (create-set-ltrack (rest l))]
    [else (cons (first l) (create-set-ltrack (rest l)))]))


; Track LTracks -> Boolean
; returns true if a given track is already in an LTracks
(check-expect (is-dupe-track? TRACK1 LTRACK1) #true)
(check-expect (is-dupe-track? TRACK1 LTRACK3) #false)
(define (is-dupe-track? t l)
  (cond
    [(empty? l) #false]
    [(and (string=? (track-name t) (track-name (first l)))
          (string=? (track-artist t) (track-artist (first l)))
          (= (track-track# t) (track-track# (first l)))) #true]
    [else (is-dupe-track? t (rest l))]))

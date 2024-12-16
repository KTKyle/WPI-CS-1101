;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname Truong-K-hw1) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;;1)

;; day is a natural
;; month is a natural
;; year is a natural

;; interp.
;; day represents an integer between (1-31)
;; month represents an integer between (1-12)
;; year represents an integer between (0-infinity)

(define-struct date (day month year))

;; examples of date:
(define DATE-1 (make-date 14 3 2009))
(define DATE-2 (make-date 2 9 2077))
(define DATE-3 (make-date 26 2 1900))

;; title is a String
;; mpaa-rating is a String
;; run-time is a Natural
;; genre is a String
;; earnings is a Natural
;; release-date is a date

;;interp.
;; title: the movie's title
;; mpaa-rating: the movie's rating 
;; run-time: the length of the movie (in minutes)
;; genre: the movie's genre
;; earnings: the weekly earnings of the movie (in dollars)
;; release-date: the release date of the movie following the (day month year) structure

(define-struct movie (title mpaa-rating run-time genre earnings release-date))

;; examples of movie:
(define MOVIE-1 (make-movie "Space Jam 3" "PG" 127 "Sports" 100 DATE-1))
(define MOVIE-2 (make-movie "The Amazing Spider-Man 3" "PG-13" 155 "Action" 200 DATE-2))
(define MOVIE-3 (make-movie "Rudolph the Blood-Nosed Reindeer" "PG-13" 180 "Horror" 7 DATE-3))

;;2)

;; movie Constructors:
;; make-movie: String String Natural String Natural Date -> movie

;; movie Selectors:
;; movie-title: movie -> String
;; extracts the title of the given movie

;; movie-mpaa-rating: movie -> String
;; extracts the mpaa rating of the given movie

;; movie-run-time: movie -> Natural
;; extracts the run time of the given movie

;; movie-genre: movie -> String
;; extracts the genre of the given movie

;; movie-earnings: movie -> Natural
;; extracts the earnings of the given movie

;; movie-release-date: movie -> date
;; extracts the date of the given movie

;; movie Predicate:
;; movie?: Any -> Boolean
;; checks if the given value is a movie

;; date Constructors:
;; make-date: Natural Natural Natural -> date

;; date Selectors:
;; date-day: date -> Natural
;; extracts the day of the given date

;; date-month: date -> Natural
;; extracts the month of the given date

;; date-year: date -> Natural
;; extracts the year of the given date

;; date Predicate:
;; date?: Any -> Boolean
;; checks if the given value is a date

;;3)

;; movie -> boolean
;; The function checks to see if a given movie's genre is Horror or the mpaa rating is either NC-17 or none

(define (nightmare-fuel? movie-A)
  (or [string=? (movie-genre movie-A) "Horror"]
      [string=? (movie-mpaa-rating movie-A) "NC-17"]
      [string=? (movie-mpaa-rating movie-A) "none"]))

(check-expect (nightmare-fuel? MOVIE-1) false)
(check-expect (nightmare-fuel? MOVIE-2) false)
(check-expect (nightmare-fuel? MOVIE-3) true)
(check-expect (nightmare-fuel? (make-movie "test" "NC-17" 190 "Horror" 1 (make-date 1 2 3))) true)
(check-expect (nightmare-fuel? (make-movie "test" "NC-17" 190 "PG" 1 (make-date 1 2 3))) true)

;;4)

;; movie movie -> number
;; The function adds the sums the run-times for the two given movies plus 15 minutes for the intermission between the showings

(define INTERMISSION 15)
(define (double-feature-length movie-B movie-C)
  (+ (movie-run-time movie-B) (movie-run-time movie-C) INTERMISSION))

(check-expect (double-feature-length MOVIE-1 MOVIE-1) 269)
(check-expect (double-feature-length MOVIE-2 MOVIE-1) 297)
(check-expect (double-feature-length MOVIE-2 MOVIE-3) 350)
(check-expect (double-feature-length MOVIE-1 MOVIE-3) 322)


;;5)

;; movie number -> movie
;; The function makes a new movie almost identical to the inputted one with the updated earnings which are the sum of the old earnings and the entered number

(define (update-earnings movie earnings)
  (make-movie (movie-title movie)(movie-mpaa-rating movie)(movie-run-time movie)(movie-genre movie) (+ (movie-earnings movie) earnings) (movie-release-date movie)))

(check-expect (update-earnings MOVIE-1 50) (make-movie "Space Jam 3" "PG" 127 "Sports" 150 (make-date 14 3 2009)))
(check-expect (update-earnings MOVIE-1 -50) (make-movie "Space Jam 3" "PG" 127 "Sports" 50 (make-date 14 3 2009)))
(check-expect (update-earnings MOVIE-1 0) (make-movie "Space Jam 3" "PG" 127 "Sports" 100 (make-date 14 3 2009)))
(check-expect (update-earnings MOVIE-1 200000000) (make-movie "Space Jam 3" "PG" 127 "Sports" 200000100 (make-date 14 3 2009)))

;;6)

;; movie movie -> string
;; The function compares if two movies were released on the same day or will use the compare date function to determine which one of them is a newer movie

(define (newer-movie movie-D movie-E)
  (if (equal? (movie-release-date movie-D) (movie-release-date movie-E))
      "Both Opened On Same Date"
     (compare-date movie-D movie-E)))

(check-expect (newer-movie MOVIE-1 MOVIE-1) "Both Opened On Same Date")
(check-expect (newer-movie MOVIE-1 MOVIE-2)"The Amazing Spider-Man 3")
(check-expect (newer-movie MOVIE-1 (make-movie "test" "test" 0 "test" 0 (make-date 0 0 0)))"Space Jam 3")
(check-expect (newer-movie MOVIE-1 (make-movie "test" "test" 0 "test" 0 (make-date -1 -1 -1)))"Space Jam 3")
(check-expect (newer-movie MOVIE-1 (make-movie "test" "test" 0 "test" 0 (make-date 0 99999999 0)))"Space Jam 3")
(check-expect (newer-movie MOVIE-1 (make-movie "test" "test" 0 "test" 0 (make-date 99999999 99999999 0)))"Space Jam 3")

;; movie movie -> string
;; The function consumes two movies and produces the title of the movie which premiered more recently

(define (compare-date movie-D movie-E)
  (cond
      [(< (date-year (movie-release-date movie-D)) (date-year (movie-release-date movie-E))) (movie-title movie-E)]
      [(> (date-year (movie-release-date movie-D)) (date-year (movie-release-date movie-E))) (movie-title movie-D)]
      [(< (date-month (movie-release-date movie-D)) (date-month (movie-release-date movie-E))) (movie-title movie-E)]
      [(> (date-month (movie-release-date movie-D)) (date-month (movie-release-date movie-E))) (movie-title movie-D)]
      [(< (date-day (movie-release-date movie-D)) (date-day (movie-release-date movie-E))) (movie-title movie-E)]
      [(> (date-day (movie-release-date movie-D)) (date-day (movie-release-date movie-E))) (movie-title movie-D)]))

(check-expect (compare-date MOVIE-1 MOVIE-2) "The Amazing Spider-Man 3")
(check-expect (compare-date MOVIE-2 MOVIE-1) "The Amazing Spider-Man 3")
(check-expect (compare-date MOVIE-1 (make-movie "test" "test" 0 "test" 0 (make-date 0 99999999 0)))"Space Jam 3")
(check-expect (compare-date MOVIE-1 (make-movie "test" "test" 0 "test" 0 (make-date 99999999 99999999 0)))"Space Jam 3")
(check-expect (compare-date MOVIE-1 (make-movie "test" "test" 0 "test" 0 (make-date 0 0 999999999)))"test")
(check-expect (compare-date MOVIE-1 (make-movie "test" "test" 0 "test" 0 (make-date 0 0 0)))"Space Jam 3")
(check-expect (compare-date MOVIE-1 (make-movie "test" "test" 0 "test" 0 (make-date -1 -1 -1)))"Space Jam 3")


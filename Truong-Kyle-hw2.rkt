;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname Truong-Kyle-hw2) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;;1)

(define-struct planet (name variety orbital-distance rings? moons))
;; a planet is a (make-planet String String Natural Boolean Natural)
;; name is the planet's name
;; variety is the type of planet it is (ex. "Gas Giant" or "Rock")
;; orbital-distance is the distance the planet is from the sun it orbits
;; rings? is if the planet has rings or not
;; moons is the number of major moons the planet has

;; Examples:
(define P1 (make-planet "Zertha" "Rock" 36 false 0))
(define P2 (make-planet "Norbus" "Gas Giant" 93 true 8))
(define P3 (make-planet "Oberos" "Water World" 912 true 0))

(define-struct moon (name host-planet orbital-period largest?))
;; a moon is a (make-moon String String Natural Boolean)
;; name is the moon's name
;; host-planet is the planet the moon orbits
;; orbital-period is how long it takes to complete one orbit in days
;; largest? is if this moon is the largest moon that orbits its host planet

;; Examples:
(define M1 (make-moon "Zorphod" "Bithua" 98 false))
(define M2 (make-moon "Tithica" "Super-Earth" 810 true))

(define-struct comet (name orbital-period year))
;; a comet is a (make-comet String Natural Natural)
;; name is the name of the comet
;; orbital-period is the time it takes to complete one orbit in days
;; year is the earliest year the comet will be visible from earth

;; Examples:
(define C1 (make-comet "rA9-Corvis" 100 2361))
(define C2 (make-comet "Ito-Junji" 75 2050))

;; Satellite can be made of:
;; (make-planet String String Natural Boolean Natural)
;; (make-moon String String Natural Boolean)
;; (make-comet String Natural Natural)

;; Examples:
(define S1 (cons P1 (cons M1 (cons C1 empty))))
(define S2 (cons P2 (cons M2 (cons C2 empty))))

;;2)

;; Templates for Satellite types:

;; planet-fcn:  Planet -> ...
;; (define (planet-fcn a-planet)
;; (... (planet-name a-planet)
;; (planet-variety a-planet)
;; (planet-orbital-distance a-planet)
;; (planet-rings? a-planet)
;; (planet-moons a-planet)))

;; moon-fcn:  Moon -> ...
;;(define (moon-fcn a-moon)
;; (... (moon-name a-moon)
;; (moon-host-planet a-moon)
;; (moon-orbital-period a-moon)
;; (moon-largest? a-moon)))

;; comet-fcn:  Comet -> ...
;; (define (comet-fcn a-comet)
;; (... (comet-name a-comet)
;; (comet-orbtial-period a-comet)
;; (comet-year a-comet))

;; Template for Satellite:
; ;; ListOfSatellite-fcn:  ListOfSatellite -> ...
; ;;
; (define (ListOfSatellite-fcn a-ListOfSatellite)
;   (cond [(empty? a-ListOfSatellite)  (...) ]
;         [(cons? a-ListOfSatellite)   (... (Satellite-fcn (first a-ListOfSatellite))
;                                           (ListOfSatellite-fcn (rest a-ListOfSatellite)))]))

;;choose one or other

;(define (ListOfSatellite-fcn a-ListOfSatellite)
;  (cond
;    [(empty? a-ListOfSatellite) (...)]
;    [(cons? a-ListOfSatellite) (... (Satellite-fcn (first a-ListOfSatellite))
;                                    (ListOfSatellite-fcn (rest a-ListOfSatellite)))])) 
;;4)

;; satellite -> Boolean
;; Function consumes a satellite and checks to see if it is a planet with neither rings or moons,
;; or a moon that is not its host planet's largest, or a comet who won't return before the year 2100

(define (uninteresting? satellite)
  (cond
    [(and (planet? satellite) (boolean=? (planet-rings? satellite) false) (= (planet-moons satellite) 0)) true]
    [(and (moon? satellite) (boolean=? (moon-largest? satellite) false)) true]
    [(and (comet? satellite) (>= (comet-year satellite) 2100)) true]
    [(or (planet? satellite) (moon? satellite) (comet? satellite)) false]))


(check-expect (uninteresting? P1) true)
(check-expect (uninteresting? P2) false)
(check-expect (uninteresting? M1) true)
(check-expect (uninteresting? M2) false)
(check-expect (uninteresting? C1) true)
(check-expect (uninteresting? C2) false)

;;5)

;; rename-satellite: Satellite String -> Satellite
;; Produces a new satellite with the same properties as the original but with the new name.

(define (rename-satellite satellite new-name)
  (cond
    [(planet? satellite) (make-planet new-name (planet-variety satellite) (planet-orbital-distance satellite) (planet-rings? satellite) (planet-moons satellite))]
    [(moon? satellite) (make-moon new-name(moon-host-planet satellite) (moon-orbital-period satellite) (moon-largest? satellite))]
    [(comet? satellite)(make-comet new-name(comet-orbital-period satellite) (comet-year satellite))]))

(check-expect (rename-satellite (make-planet "test" "test" 0 true 0) "Booger")(make-planet "Booger" "test" 0 true 0))
(check-expect (rename-satellite C1 "Death Star") (make-comet "Death Star" 100 2361))
(check-expect (rename-satellite M2 "Auridon") (make-moon "Auridon" "Super-Earth" 810 true))
(check-expect (rename-satellite P1 "Zertha") (make-planet "Zertha" "Rock" 36 false 0))
(check-expect (rename-satellite M1 "Zorphod-!)@($&(*#@!&")(make-moon "Zorphod-!)@($&(*#@!&" "Bithua" 98 false))
(check-expect (rename-satellite (make-planet "test" "test" 0 true 0) "")(make-planet "" "test" 0 true 0))
(check-expect (rename-satellite (make-planet "test" "test" 0 true 0) "              ")(make-planet "              " "test" 0 true 0))

;;6)
;; ListOfString -> Boolean
;; Returns true if any string in the list is in all uppercase letters.

(define (any-shouting? temp-list)
  (cond
    [(empty? temp-list) false]
    [(string=? (first temp-list) (string-upcase (first temp-list)))true]
    [(cons? temp-list) (any-shouting? (rest temp-list))]))

(check-expect (any-shouting? empty) false)
(check-expect (any-shouting? (list "HELLO" "WORLD")) true)
(check-expect (any-shouting? (list "hello" "WORLD")) true)
(check-expect (any-shouting? (list "hello" "world")) false)
(check-expect (any-shouting? (list "hello")) false)
(check-expect (any-shouting? (list "HELLO")) true)
(check-expect (any-shouting? (list "hello" "" "world" "TEST")) true)


;;7)

;; ListOfString -> Natural
;; Counts the amount of items in the list that start with either "K" or "k"

(define (kount-k-words list)
  (cond
    [(empty? list) 0]
    [(string=? (substring (string-upcase (first list)) 0 1) "K") (+ 1 (kount-k-words (rest list)))]
    [(cons? list) (kount-k-words (rest list))]))

(check-expect (kount-k-words empty) 0) 
(check-expect (kount-k-words (cons "knock" (cons "clock" (cons "Planet" (cons "captain" (cons "kitten" (cons "Kit-Kat" empty))))))) 3) ; 
(check-expect (kount-k-words (cons "apple" (cons "banana" (cons "kiwi" (cons "Kite" empty))))) 2) 
(check-expect (kount-k-words (cons "karma" (cons "cookie" (cons "pick" empty)))) 1) 


;;8)

;; ListOfString -> ListOfNatural
;; The function produces a list of the lengths of the strings in the given ListOfString in their respective order

(define (list-of-lengths list)
  (cond [(empty? list) empty]
        [(cons? list) (cons (string-length (first list))
                    (list-of-lengths (rest list)))]))

(check-expect (list-of-lengths empty) empty)
(check-expect (list-of-lengths (cons "one" (cons "two" (cons "three" (cons "four" empty)))))(cons 3 (cons 3 (cons 5 (cons 4 '())))))
(check-expect (list-of-lengths (cons "test" (cons "" (cons "number" empty))))(cons 4 (cons 0 (cons 6 '()))))
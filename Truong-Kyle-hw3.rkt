;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname Truong-Kyle-hw3) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;;1)
(define-struct goodie (name price number-ordered classification healthy?))
;; a goodie is a (make-goodie String Natural Natural String Boolean)
;; name is the name of the food/drink item
;; price is the price of the goodie
;; number-ordered is the index of the order for the item
;; classification is what type of food/drink it is (burger, side, salad, drink, etc.)
;; healthy? represents if the goodie is healthy or not

;;Examples

(define G1 (make-goodie "Chocolate Cake" 5 2 "dessert" true))
(define G2 (make-goodie "Cheesecake" 6 1 "dessert" false))
(define G3 (make-goodie "Fruit Salad" 4 3 "dessert" true))
(define G4 (make-goodie "Burger" 12 1 "burger" false))
(define G5 (make-goodie "Wagyu Steak" 120 1 "steak" false))
(define G6 (make-goodie "Caesar Salad" 8 1 "salad" true))
(define G7 (make-goodie "Goat Cheese" 4 1 "goat cheese" true))

;;2)

;; Template for goodie Functions:

;; (define (goodie-fcn a-goodie)
;; (... (goodie-name a-goodie)
;; (goodie-price a-goodie)
;; (goodie-number-ordered a-goodie)
;; (goodie-classification a-goodie)
;; (goodie-healthy? a-goodie)))

;;3)

;; a Trayful is one of
;; empty
;; (cons goodie ListOfgoodie)

;;Examples:
(define T1 (list G1 G2 G3 G4)) 
(define T2 (list G4 G5 G6))
(define T3 (list G2 G3 G5 G6))
(define T4 (list G4 G3 G1 G7 G2))

;;4)

;; Template for 
;; ListOfGoodie-fcn:  ListOfGoodie -> ...

;; (define (ListOfGoodie-fcn alog)
;;   (cond [(empty? alog)  (...) ]
;;         [(cons? alog)   (...(goodie-fcn (first alog))
;;                             (ListOfGoodie-fcn (rest alog)))]))


;;5)

;; Trayful Natural -> Trayful
;; consumes a trayful and a number and returns a trayful of salads that are no greater than the price of the given number

(define (list-bargain-salads trayful price)
  (cond [(empty? trayful) empty]
        [(and (string=? (goodie-classification (first trayful)) "salad") (<= (goodie-price (first trayful)) price))
         (cons (first trayful) (list-bargain-salads (rest trayful) price))]
        [(cons? trayful) (list-bargain-salads (rest trayful) price)]))

(check-expect (list-bargain-salads T3 10) (list (make-goodie "Caesar Salad" 8 1 "salad" true)))
(check-expect (list-bargain-salads T3 8) (list (make-goodie "Caesar Salad" 8 1 "salad" true)))
(check-expect (list-bargain-salads T3 7) empty)
(check-expect (list-bargain-salads T1 10) empty)
(check-expect (list-bargain-salads empty 10) empty)

;;6)

;; Trayful -> Boolean
;; consumes a trayful and returns true if no goodies included in the trayful have a quantity greater than one

(define (all-solo-trayful? trayful)
  (cond [(empty? trayful) true]
        [(cons? trayful) (and (= (goodie-number-ordered (first trayful)) 1)
                              (all-solo-trayful? (rest trayful)))]))

(check-expect (all-solo-trayful? T2) true)
(check-expect (all-solo-trayful? empty) true)
(check-expect (all-solo-trayful? T1) false)

;;7)

;; Trayful -> Natural
;; consumes a trayful and produces the total number of all healthy desserts ordered

(define (count-healthy-desserts trayful)
  (cond
    [(empty? trayful) 0] 
    [(and (string=? (goodie-classification (first trayful)) "dessert") (goodie-healthy? (first trayful)))
     (+ (goodie-number-ordered (first trayful))(count-healthy-desserts (rest trayful)))]
    [(cons? trayful)(count-healthy-desserts (rest trayful))]))

(check-expect (count-healthy-desserts empty) 0)
(check-expect (count-healthy-desserts T1) 5)
(check-expect (count-healthy-desserts T2) 0)

;;8)

;; Trayful -> Natural
;; consumes a trayful and produces the total cost of all the goodies on it

(define (trayful-total trayful)
  (cond
    [(empty? trayful) 0]
    [(cons? trayful) (+ (* (goodie-price (first trayful))(goodie-number-ordered (first trayful))) (trayful-total (rest trayful)))]))

(check-expect (trayful-total T1) 40)
(check-expect (trayful-total T2) 140)
(check-expect (trayful-total empty) 0)

;;9)

;; Trayful Natural -> Natural
;; consumes a trayful and a natural (which represents the decimal value of the discount applied on goat cheese items)
;; and produces the total cost of the trayful with the discount applied to goat cheese goodies in the trayful

(define (goat-cheese-promo trayful discount)
  (cond 
    [(empty? trayful) 0]
    [(string=? (goodie-classification (first trayful)) "goat cheese")
     (+ (* (- 1 discount) (* (goodie-price (first trayful)) (goodie-number-ordered (first trayful))))(goat-cheese-promo (rest trayful) discount))]
    [(cons? trayful)(+ (* (goodie-price (first trayful)) (goodie-number-ordered (first trayful)))(goat-cheese-promo (rest trayful) discount))]))

(check-expect (goat-cheese-promo empty 0.65) 0)
(check-expect (goat-cheese-promo T4 0.20) 43.2)
(check-expect (goat-cheese-promo T4 0) 44)
(check-expect (goat-cheese-promo T3 0.15) 146)

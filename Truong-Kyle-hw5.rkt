;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname Truong-Kyle-hw5) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;;1)

(define-struct river (name bloom? ph tributaries))
;; A river is a (make-river String Boolean Natural ListOfRiver)
;; - name: The name of the river
;; - bloom?: Is there a presence of algal blooms or not
;; - ph: The pH of the water
;; - tributaries: A list of river systems that feeds into the main river

;; A ListOfRiver is one of:
;; - empty
;; - (cons River ListOfRiver)


;;2)

;; Examples:

(define JEFFERSON (make-river "Jefferson River" true 10.1 (list (make-river "Beaverhead River" false 6.9 empty)
                                                                (make-river "Big Hole River" true 9.8 empty))))

(define YELLOWSTONE (make-river "Yellowstone River" false 8.5 (list (make-river "Gardner River" true 2.5 empty)
                                                                    (make-river "Shields River" true 4.1 empty)
                                                                    (make-river "Boulder River" false 7.5 empty))))

(define MISSOURI (make-river "Missouri River" false 7.3 (list JEFFERSON
                                                              (make-river "Sun River" true 6.2 empty)
                                                              YELLOWSTONE
                                                              (make-river "Madison River" false 8.4 empty)
                                                              (make-river "Gallatin River" false 7.7 empty))))

;;3)

;; Templates

;;; river-fcn : river -> ...
;(define (river-fcn a-river)
;  (... (river-name a-river)
;       (river-bloom? a-river)
;       (river-ph a-river)
;       (ListOfRiver-fcn (river-tributaries a-river))))
;
;;; ListOfRiver-fcn : ListOfRiver -> ...
;(define (ListOfRiver-fcn a-LoR)
;  (cond [(empty? a-LoR) ...]
;        [(cons? a-LoR) ... (river-fcn (first a-LoR))
;                       ... (ListOfRiver-fcn (rest a-LoR))]))



;;4)

;; list-clean-rivers : river -> ListOfString
;;The function returns a list of the names of rivers in the system that do not have algal blooms

(define (list-clean-rivers a-river)
  (local [(define clean-tributaries
            (apply append (map list-clean-rivers (river-tributaries a-river))))] 
    (if (not (river-bloom? a-river))
        (cons (river-name a-river) clean-tributaries)
        clean-tributaries))) 

;; check-expects:
(check-expect (list-clean-rivers MISSOURI) (list "Missouri River" "Beaverhead River" "Yellowstone River" "Boulder River" "Madison River" "Gallatin River"))
(check-expect (list-clean-rivers JEFFERSON)(list "Beaverhead River"))
(check-expect (list-clean-rivers (make-river "Test River" false 7.0 empty)) (list "Test River"))
(check-expect (list-clean-rivers (make-river "Empty" true 7.0 empty)) empty)

;;5)

;; any-too-basic? : river -> Boolean
;;The function returns true if any river in the system has a pH of 8.5 or higher

(define PH-CONSTANT 8.5)
(define (any-too-basic? a-river)
  (local [(define (too-basic-in-system river)
            (or (>= (river-ph river) PH-CONSTANT)   
                (ormap too-basic-in-system (river-tributaries river))))] 
    (too-basic-in-system a-river)))

;; check-expects:
(check-expect (any-too-basic? MISSOURI) true)
(check-expect (any-too-basic? YELLOWSTONE) true)
(check-expect (any-too-basic? (make-river "Test River" false 7.0 empty)) false)

;;6)
(define RAISE-PH 0.15)
(define (raise-all-ph a-river)
  (local [(define (raise-tributary-ph tributary)
            (raise-all-ph tributary))]
    (make-river (river-name a-river) (river-bloom? a-river) (+ (river-ph a-river) RAISE-PH) 
                (map raise-tributary-ph (river-tributaries a-river)))))

;; check-expects:
(check-expect (raise-all-ph MISSOURI)
              (make-river "Missouri River" false 7.45
                          (list (make-river "Jefferson River" true 10.25
                                            (list (make-river "Beaverhead River" false 7.05 empty)
                                                  (make-river "Big Hole River" true 9.95 empty)))
                                (make-river "Sun River" true 6.35 empty)
                                (make-river "Yellowstone River" false 8.65
                                            (list (make-river "Gardner River" true 2.65 empty)
                                                  (make-river "Shields River" true 4.25 empty)
                                                  (make-river "Boulder River" false 7.65 empty)))
                                (make-river "Madison River" false 8.55 empty)
                                (make-river "Gallatin River" false 7.85 empty))))
(check-expect (raise-all-ph JEFFERSON)
              (make-river "Jefferson River" true 10.25
                          (list (make-river "Beaverhead River" false 7.05 empty)
                                (make-river "Big Hole River" true 9.95 empty))))
(check-expect (raise-all-ph (make-river "test" false 7.0 empty)) 
              (make-river "test" false 7.15 empty))
(check-expect (raise-all-ph (make-river "test" false 8.5 empty)) 
              (make-river "test" false 8.65 empty))

;;7)

;; find-subsystem : String river -> river or false
;; The function returns the portion of the original river system that has the named river as its root or false if there is none

(define (find-subsystem name a-river)
  (local [(define (find-in-tributaries tributaries)
            (let ([CORRECT-NAME (filter (lambda (a-tributary) (string=? name (river-name a-tributary))) tributaries)])
              (if (empty? CORRECT-NAME)
                  false
                  (first CORRECT-NAME))))]
    (if (string=? name (river-name a-river))
        a-river                                  
        (find-in-tributaries (river-tributaries a-river)))))


;; check-expects:
(check-expect (find-subsystem "Yellowstone River" MISSOURI) YELLOWSTONE)
(check-expect (find-subsystem "Jefferson River" MISSOURI) JEFFERSON)
(check-expect (find-subsystem "Test" MISSOURI) false)


;; Part 2:

(define-struct goodie (name price number-ordered classification healthy?))
;; a Goodie is a (make-goodie String Number Natural String Boolean)
;; - name of the food/drink item
;; - price of a single item
;; - number-ordered for the item quantity
;; - classification includes: "burger" "side" "goat cheese" "dessert" "beverage" etc.
;; - healthy? is true if the goodie is good(ie) for you

;; a Trayful is one of:
;; empty
;; (cons Goodie Trayful)

;;8)

;; list-bargain-salads : Trayful Number -> Trayful
;; consumes a trayful and the threshold cost
;; produces a trayful only containing goodies that are salads and
;; price is <= given number

(define (list-bargain-salads trayful cost)
  (local [(define (is-bargain-salad goodie)
            (and (string=? (goodie-classification goodie) "salad") (<= (goodie-price goodie) cost)))]
    (filter is-bargain-salad trayful)))
 
;; check-expects:
(define TRAY1 (list (make-goodie "Caesar Salad" 5.00 1 "salad" true)
                    (make-goodie "Cheese Burger" 8.00 2 "burger" false)
                    (make-goodie "Onion Salad" 6.50 1 "salad" true)
                    (make-goodie "Fries" 3.00 3 "side" false)))

(check-expect (list-bargain-salads TRAY1 6.00) (list (make-goodie "Caesar Salad" 5 1 "salad" #true)))
(check-expect (list-bargain-salads TRAY1 7.00) (list (make-goodie "Caesar Salad" 5.00 1 "salad" true) (make-goodie "Onion Salad" 6.50 1 "salad" true)))
(check-expect (list-bargain-salads TRAY1 3.00) empty)

;;9)

;; all-solo-trayful? : Trayful -> Boolean
;; consumes a trayful and produces true
;; if no goodies on the tray have a quantity > 1

(define (all-solo-trayful? trayful)
  (not (ormap (lambda (g) (> (goodie-number-ordered g) 1)) trayful)))

;; check-expects:
(define TRAY2 (list (make-goodie "Caesar Salad" 5.00 1 "salad" true)
                    (make-goodie "Greek Salad" 6.50 1 "salad" true)))

(check-expect (all-solo-trayful? TRAY2) true)
(check-expect (all-solo-trayful? TRAY1) false)
(check-expect (all-solo-trayful? empty) true)

;;10)

(define (goat-cheese-varieties trayful)
  (local [(define (is-goat-cheese goodie)
            (string=? (goodie-classification goodie) "goat cheese"))]
    (map goodie-name (filter is-goat-cheese trayful))))

;; check-expects:
(define TRAY3 (list (make-goodie "Goat Cheese Salad" 7.00 1 "goat cheese" true)
                    (make-goodie "Caesar Salad" 5.00 1 "salad" true)
                    (make-goodie "Feta Cheese Salad" 6.00 2 "goat cheese" false)))

(check-expect (goat-cheese-varieties TRAY3) (list "Goat Cheese Salad" "Feta Cheese Salad"))
(check-expect (goat-cheese-varieties TRAY1) empty)

;; Bonus)

;; trayful-total : Trayful -> Number
;; consumes a trayful and produces the total cost of all the goodies, including multiples

(define (trayful-total trayful)
  (foldr (lambda (goodie acc)
            (+ (* (goodie-price goodie) (goodie-number-ordered goodie)) acc)) 0 trayful))

;; check-expects:
(check-expect (trayful-total TRAY1) 36.5)
(check-expect (trayful-total empty) 0)
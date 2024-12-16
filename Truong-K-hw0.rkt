;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname Truong-K-hw0) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
;A)
;(+ (* (- 7 11)(- 9 (string-length (string-append "CS" "1101"))))(/ 52 4))
;(+(* -4 (- 9 (string-length("CS1101")))(13))
;(+((* -4)(- 9 6))(13))
;(+(* -4 3)(13))
;(+ -12 13)
;1

;B)
;(define (cube n)(* n n n))
;(if (> (cube 3) (sqr (/ 15 3)))(string-length "Gompei Rulezzzz!")(sqrt (string-length "Gompei Rulezzzz!")))
;(if (> (27)(25))(16)(sqrt(16)))
;(if(> 27 25) 16 4)
;(true(16 4))
;16

;2)

(define (swift-x  my-image)
  (above (flip-vertical (beside my-image (flip-horizontal my-image))) (beside my-image (flip-horizontal my-image))))

;3)
(define COST-ADULT 7)
(define COST-CHILDREN 5)
(define COST-WEE 3)

(define (outlays adults children wee)
  (+ (* adults COST-ADULT) (* children COST-CHILDREN) (* wee COST-WEE) 1500))

(define TICKET-ADULT 60)
(define TICKET-CHILDREN 45)
(define TICKET-WEE 25)
(define (income adults children wee)
  
  (+ (* adults TICKET-ADULT) (* children TICKET-CHILDREN)(* wee TICKET-WEE)))

(define (net-profit adults children wee)
  (- (income adults children wee) (outlays adults children wee)))
;4)
(define INTEREST-RATE 0.08)
(define FEE 1500)
(define (PRINCIPAL purchase-price down-payment)
  (- purchase-price down-payment))
(define LOAN-LENGTH 20)

(define (total-cost-of-house purchase-price down-payment)
  (+ FEE purchase-price (* (PRINCIPAL purchase-price down-payment) LOAN-LENGTH INTEREST-RATE)))
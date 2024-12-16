;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname Truong-Kyle-hw6) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
;;1)

(define-struct mailbox (user-name account))
;; A mailbox is a (make-mailbox String ListOfMessage)
;; - user-name is the sender's user-name 
;; - account is a list of messages

(define-struct message (user-name text retrieved?))
;; A message is a (make-message String String Boolean)
;; - user-name is the sender's user-name 
;; - text is the body of the message 
;; - retrieved? is whether or not the user has recieved the message or not

;; A ListOfMessage is one of:
;; - empty
;; - (cons message ListOfMessage)

;;2)

;; sample-system : ListOfMailbox
;; sample-system is a mutable variable representing the email system.
;; It holds a list of mailboxes, initially empty empty, indicating that there are no mailboxes in the system.
(define sample-system empty)

;;3)

;; add-mailbox : String -> (void)
;; The function consumes a user-name and produces void
;; Effect: Adds a new mailbox into a system of mailboxes

(define (add-mailbox user-name)
  (set! sample-system (cons (make-mailbox user-name empty) sample-system)))

(add-mailbox "Gompei")
(add-mailbox "Attila")
(add-mailbox "Bristaco")
(add-mailbox "Buzz")

;;4)

;; send-message : String String String -> (void)
;; consumes the user-name of the sender, the user-name of the recipient, and the text of a message, and produces void
;; Effect: Stores a new unretrieved message in the users mailbox

(define (find-mailbox user-name system)
  (cond
    [(empty? system) false]
    [(string=? (mailbox-user-name (first system)) user-name) (first system)]
    [else (find-mailbox user-name (rest system))]))

(define (send-message sender recipient text)
  (let ([mailbox (find-mailbox recipient sample-system)])
    (set-mailbox-account! mailbox
                          (cons (make-message sender text false)
                                (mailbox-account mailbox)))))

(send-message "Gompei" "Attila" "Goat big or goat home!")
(send-message "Attila" "Gompei" "You must be quackers!")
(send-message "Gompei" "Attila" "You have goat to be kidding!")
(send-message "Bristaco" "Buzz" "You look like you need a hug!")
(send-message "Buzz" "Bristaco" "Buzz off!")

;;5)

;; get-unread-messages : String -> ListOfMessages
;; The function produces a list which contains the unretrieved messages in the given account
;; Effect: set retrieved? to true

(define (get-unread-messages user-name)
  (let ([mailbox (find-mailbox user-name sample-system)])
    (let ([unread-messages (filter (lambda (msg) (not (message-retrieved? msg)))(mailbox-account mailbox))])
      (local [(define (set-messages-retrieved messages)
                (cond [(empty? messages) (void)]
                      [else (begin
                              (set-message-retrieved?! (first messages) true)
                              (set-messages-retrieved (rest messages)))]))]
        (begin (set-messages-retrieved unread-messages) unread-messages)))))

(get-unread-messages "Gompei")
(get-unread-messages "Attila")
(get-unread-messages "Bristaco")
(get-unread-messages "Buzz")

;;6)

;; most-messages : -> Mailbox
;; The function produces the mailbox in the email system with the largest number of messages.
;; If there are no mailboxes in the system, it produces an error.
;; Effect: None

(define (most-messages)
  (cond [(empty? sample-system) (error "No mailboxes in the system")]
        [else (local [(define (find-most-messages system max-mailbox)
                        (cond [(empty? system) max-mailbox]
                              [else (let ([current-mailbox (first system)])
                                     (if (> (length (mailbox-account current-mailbox)) (length (mailbox-account max-mailbox)))
                                         (find-most-messages (rest system) current-mailbox)
                                         (find-most-messages (rest system) max-mailbox)))]))]
                (find-most-messages (rest sample-system) (first sample-system)))]))

(most-messages)

;;8)

;; kount-k-words1 : ListOfString -> Natural
;; Counts the amount of items in the list that start with either "k"

(define (kount-k-words list)
  (local [(define (kount-k-words-acc list acc)
            (cond
              [(empty? list) acc]
              [(string=? (substring (string-upcase (first list)) 0 1) "K") 
               (kount-k-words-acc (rest list) (+ 1 acc))]
              [else 
               (kount-k-words-acc (rest list) acc)]))]
    (kount-k-words-acc list 0)))

(check-expect (kount-k-words empty) 0) 
(check-expect (kount-k-words (cons "knock" (cons "clock" (cons "Planet" (cons "captain" (cons "kitten" (cons "Kit-Kat" empty))))))) 3)
(check-expect (kount-k-words (cons "apple" (cons "banana" (cons "kiwi" (cons "Kite" empty))))) 2)
(check-expect (kount-k-words (cons "karma" (cons "cookie" (cons "pick" empty)))) 1)

;; Bonus 8)

;; kount-k-words-hof : ListOfString -> Natural
;; Counts the amount of items in the list that start with either "k"

(define (kount-k-words-hof list)
  (length (filter (lambda (check) (string=? (substring (string-upcase check) 0 1) "K"))  list)))

(check-expect (kount-k-words-hof empty) 0) 
(check-expect (kount-k-words-hof (cons "knock" (cons "clock" (cons "Planet" (cons "captain" (cons "kitten" (cons "Kit-Kat" empty))))))) 3)
(check-expect (kount-k-words-hof (cons "apple" (cons "banana" (cons "kiwi" (cons "Kite" empty))))) 2) 
(check-expect (kount-k-words-hof (cons "karma" (cons "cookie" (cons "pick" empty)))) 1) 

;;9)

;; ListOfString -> ListOfNatural
;; The function produces a list of the lengths of the strings in the given ListOfString in their respective order

(define (list-of-lengths list)
  (local [(define (list-of-lengths-acc list)
            (cond
              [(empty? list) empty]
              [else (cons (string-length (first list))
                          (list-of-lengths-acc (rest list)))]))]
    (list-of-lengths-acc list)))

(check-expect (list-of-lengths empty) empty)
(check-expect (list-of-lengths (list "one" "two" "three" "four")) (list 3 3 5 4))
(check-expect (list-of-lengths (list "test" "" "number")) (list 4 0 6))

;; Bonus 9)

;; list-of-lengths-hof : ListOfString -> ListOfNatural
;; The function produces a list of the lengths of the strings in the given ListOfString in their respective order

(define (list-of-lengths-hof list)
  (map string-length list))

(check-expect (list-of-lengths-hof empty) empty)
(check-expect (list-of-lengths-hof (list "one" "two" "three" "four")) (list 3 3 5 4))
(check-expect (list-of-lengths-hof (list "test" "" "number")) (list 4 0 6))

;;10)

;; total-characters : ListOfString -> Natural
;; The function produces sum of the lengths of all the strings in the given ListOfString

(define (total-characters list)
  (local [(define (total-characters-acc list acc)
            (cond
              [(empty? list) acc]  
              [else (total-characters-acc (rest list) (+ acc (string-length (first list))))]))]
    (total-characters-acc list 0)))

(check-expect (total-characters empty) 0)
(check-expect (total-characters (list "one" "two" "three")) 11)  
(check-expect (total-characters (list "test" "" "number")) 10)  

;; Bonus 10)

;; total-characters : ListOfString -> Natural
;; The function produces sum of the lengths of all the strings in the given ListOfString

(define (total-characters-hof list)
  (apply + (map string-length list)))  

(check-expect (total-characters-hof empty) 0)
(check-expect (total-characters-hof (list "one" "two" "three")) 11) 
(check-expect (total-characters-hof (list "test" "" "number")) 10)  
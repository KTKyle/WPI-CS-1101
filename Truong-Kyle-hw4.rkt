;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname Truong-Kyle-hw4) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;;1)

(define-struct student (name email-address))
;; A Student is a structure: (make-student String String)
;; name is the student's name
;; email-address is the student's email address

;; Example Students:
(define JOHN (make-student "John" "jimac@wpi.edu"))
(define JANE (make-student "Jane" "jpwilkins@wpi.edu"))
(define XAVIER (make-student "Xavier" "xgcollins@wpi.edu"))
(define KEVIN (make-student "Kevin" "klngyuen@wpi.edu"))
(define VIVIAN (make-student "Vivian" "vlngyuen@wpi.edu"))

;; A ListOfStudent is one of:
;; - empty
;; - (cons Student ListOfStudent)

;; Example ListOfStudent:
(define L1 (list JOHN XAVIER KEVIN))
(define L2 (list JANE VIVIAN))
(define L3 (list (make-student "Jimmy" "jlpricket@wpi.edu") (make-student "Marissa" "mrzakza@wpi.edu") (make-student "Cindy" "cbwilliams@wpi.edu")))
(define L4 (list (make-student "Michael" " macarter@wpi.edu") (make-student "Emily" "erthompson@wpi.edu") (make-student "Jessica" " jmbrown@wpi.edu") (make-student "Connor" "crandrid@wpi.edu")))
(define L5 (list (make-student "Daniel" "dtmoore@wpi.edu") (make-student "Kara" "krmitchell@wpi.edu")))
(define L6 (list (make-student "Zoe" "zprichardson@wpi.edu") (make-student "Markus" "mjmorgan@wpi.edu") (make-student "Dakota" "dicooper@wpi.edu")))
(define L7 (list (make-student "Hank" "hofisher@wpi.edu") (make-student "Mia" "macaldwell@wpi.edu") (make-student "Aiden" "aacaldwell@wpi.edu")))

(define-struct projectnode (project-id-num advisor title students category left right))
;; a ProjectNode is a (make-projectnode Number String String ListOfStudent String BST BST)

;; interp: false means no BST or empty BST
;; - project-id-num represents a department and a project within that department
;; - advisor is the name of the student's project advisor
;; - title is the title of the student's project
;; - students is a ListOfStudents who are also working on the same project
;; - category represents the type of project the student is involved in (Ex. "IQP", "MQP", "ISP", etc.)
;; - left is the left subtree
;; - right is the right subtree

;; a BST is one of
;; - false
;; - ProjectNode

;; Example projectnodes:
(define NODE1 (make-projectnode 42.205 "Professor Waltz" "Project A" L1 "IQP" false false))
(define NODE2 (make-projectnode 39.173 "Professor Jameson" "Project B" L2 "empty" false false))
(define NODE3 (make-projectnode 47.386 "Professor Igor" "Project C" L3 "MQP" false false))
(define NODE4 (make-projectnode 36.114 "Professor Jones" "Project D" L4 "ISP" false false))
(define NODE5 (make-projectnode 40.198 "Professor Jung" "Project E" L5 "IQP" false false))
(define NODE6 (make-projectnode 51.232 "Professor Benidict" "Project F" L6 "ISP" false false))
(define NODE7 (make-projectnode 48.813 "Professor Smith" "Project G" L7 "MQP" false false))

;; The invariant for this BST is:
;; - All project IDs in the left subtree are < the project ID of the current node.
;; - All project IDs in the right subtree are > the project ID of the current node.
;; - The same project ID never appears twice in the tree

;;2)

;; Example Binary Search Tree:
(define WPI-BST
  (make-projectnode 42.205 "Professor Waltz" "Project A" L1 "IQP"
                    (make-projectnode 39.173 "Professor Jameson" "Project B" L2 "empty"
                                      (make-projectnode 36.114 "Professor Jones" "Project D" L4 "ISP" false false)
                                      (make-projectnode 40.198 "Professor Jung" "Project E" L5 "IQP" false false))
                    (make-projectnode 47.386 "Professor Igor" "Project C" L3 "MQP" false
                                      (make-projectnode 51.232 "Professor Benidict" "Project F" L6 "ISP"
                                                        (make-projectnode 51.118 "Professor Smith" "Project G" L7 "MQP" false false) false))))

(define BST2
  (make-projectnode 42.2405 "Professor Adams" "Project 1" L1 "MQP"
                    (make-projectnode 10.1234 "Professor Brown" "Project 2" L2 "IQP" false 
                                      (make-projectnode 30.5432 "Professor Davis" "Project 3" empty "ISP" false false))
                    (make-projectnode 55.6789 "Professor Clark" "Project 4" L3 "MQP" false 
                                      (make-projectnode 70.9876 "Professor Evans" "Project 5" L4 "IQP" false false))))

;;3)

;;; student-fcn: Student -> ...
;(define (student-fcn a-student)
;  (... (student-name a-student)
;  (student-email-address a-student)))
;
;;; ListOfStudent-fcn: ListOfStudent -> ...
;(define (ListOfStudent-fcn a-ListOfStudent)
;  (cond
;    [(empty? a-ListOfStudent) ...]
;    [(cons? a-ListOfStudent) ... (student-fcn (first a-ListOfStudent))
;                             ... (ListOfStudent-fcn (rest a-ListOfStudent))]))
;
;;; projectnode-fcn: ProjectNode -> ...
;(define (projectnode-fcn a-node)
;  (projectnode-project-id-num a-node)
;  (projectnode-advisor a-node)
;  (projectnode-title a-node)
;  (ListOfStudent-fcn (projectnode-students a-node))
;  (projectnode-category a-node)
;  (bst-fcn (projectnode-left a-node))
;  (bst-fcn (projectnode-right a-node)))
;
;;; bst-fcn: BST -> ...
;(define (bst-fcn a-bst)
;  (cond
;    [(false? a-bst) ...]
;    [(projectnode? a-bst) (bst-fcn a-bst)]))

;;4)

;; rename-project: BST Natural String -> BST
;; renames the title of the project with the given project-id, if it exists in the BST

(define (rename-project BST proj-num title)
  (cond
    [(= (projectnode-project-id-num BST) proj-num)
     (make-projectnode (projectnode-project-id-num BST)
                       (projectnode-advisor BST)
                       title
                       (projectnode-students BST)
                       (projectnode-category BST)
                       (projectnode-left BST)
                       (projectnode-right BST))]
    [(< proj-num (projectnode-project-id-num BST))
     (make-projectnode (projectnode-project-id-num BST)
                       (projectnode-advisor BST)
                       (projectnode-title BST)
                       (projectnode-students BST)
                       (projectnode-category BST)
                       (rename-project (projectnode-left BST) proj-num title)
                       (projectnode-right BST))]
    [(projectnode? BST)
     (make-projectnode (projectnode-project-id-num BST)
                       (projectnode-advisor BST)
                       (projectnode-title BST)
                       (projectnode-students BST)
                       (projectnode-category BST)
                       (projectnode-left BST)
                       (rename-project (projectnode-right BST) proj-num title))]))

 
;; check-expects:
(check-expect (rename-project WPI-BST 42.205 "AI In Food") (make-projectnode 42.205 "Professor Waltz" "AI In Food" L1 "IQP"
                                                                             (make-projectnode 39.173 "Professor Jameson" "Project B" L2 "empty"
                                                                                               (make-projectnode 36.114 "Professor Jones" "Project D" L4 "ISP" false false)
                                                                                               (make-projectnode 40.198 "Professor Jung" "Project E" L5 "IQP" false false))
                                                                             (make-projectnode 47.386 "Professor Igor" "Project C" L3 "MQP" false
                                                                                               (make-projectnode 51.232 "Professor Benidict" "Project F" L6 "ISP"
                                                                                                                 (make-projectnode 51.118 "Professor Smith" "Project G" L7 "MQP" false false) false))))

(check-expect (rename-project WPI-BST 51.118 "Virtual Tour of Everest") (make-projectnode 42.205 "Professor Waltz" "Project A" L1 "IQP"
                                                                                          (make-projectnode 39.173 "Professor Jameson" "Project B" L2 "empty"
                                                                                                            (make-projectnode 36.114 "Professor Jones" "Project D" L4 "ISP" false false)
                                                                                                            (make-projectnode 40.198 "Professor Jung" "Project E" L5 "IQP" false false))
                                                                                          (make-projectnode 47.386 "Professor Igor" "Project C" L3 "MQP" false
                                                                                                            (make-projectnode 51.232 "Professor Benidict" "Project F" L6 "ISP"
                                                                                                                              (make-projectnode 51.118 "Professor Smith" "Virtual Tour of Everest" L7 "MQP" false false) false))))

;;5)

;; number-of-students-in-dept: BST Natural -> Natural
;; consumes a binary search tree and the number of a department, and produces the number of students working on projects (of any category) that are from that department

(define (number-of-students-in-dept a-node department-id)
  (cond
    [(false? a-node) 0] 
    [(= (floor (projectnode-project-id-num a-node)) department-id)
     (+ (length (projectnode-students a-node))
        (number-of-students-in-dept (projectnode-left a-node) department-id)
        (number-of-students-in-dept (projectnode-right a-node) department-id))]
    [(projectnode? a-node)
     (+ (number-of-students-in-dept (projectnode-left a-node) department-id)
        (number-of-students-in-dept (projectnode-right a-node) department-id))]))

;; check-expects:
(check-expect (number-of-students-in-dept WPI-BST 40) 2)
(check-expect (number-of-students-in-dept WPI-BST 51) 6)
(check-expect (number-of-students-in-dept BST2 30) 0)
(check-expect (number-of-students-in-dept WPI-BST 0) 0) 


;;6)

;; student-in-cur-list?: String ListOfStudent -> Boolean
;; checks if the email given matches with any student in the given list

(define (student-in-cur-list? email list)
  (cond
    [(empty? list) false]
    [(equal? (student-email-address (first list)) email) true]
    [(cons? list) (student-in-cur-list? email (rest list))]))

;; check-expects:
(check-expect (student-in-cur-list? "jimac@wpi.edu" L1) true)
(check-expect (student-in-cur-list? "test@wpi.edu" L1) false) 


;; student-has-MQP?: BST String -> Boolean
;; the function returns true if the given email address appears in the list of students working on any MQP and false otherwise

(define (student-has-MQP? a-node email)
  (cond [(false? a-node) false]
        [(and (string=? (projectnode-category a-node) "MQP") (student-in-cur-list? email (projectnode-students a-node))) true]
        [(projectnode? a-node)
         (or 
          (student-has-MQP? (projectnode-left a-node) email)
          (student-has-MQP? (projectnode-right a-node) email))]))


(check-expect (student-has-MQP? WPI-BST "vlngyuen@wpi.edu") false)
(check-expect (student-has-MQP? WPI-BST "mrzakza@wpi.edu") true)
(check-expect (student-has-MQP? WPI-BST "test@wpi.edu") false)


;;7)

;; project-list-ordered-by-id-num: BST -> ListOfString
;; consumes a binary search tree and produces a list of the titles of the projects, sorted in order by ascending project number

(define (project-list-ordered-by-id-num a-node)
  (cond [(false? a-node) empty]
        [(projectnode? a-node)
         (append (project-list-ordered-by-id-num (projectnode-left a-node))
                 (list (projectnode-title a-node))
                 (project-list-ordered-by-id-num (projectnode-right a-node)))]))

;; check-expects:
(check-expect (project-list-ordered-by-id-num WPI-BST) (list "Project D" "Project B" "Project E" "Project A" "Project C" "Project G" "Project F"))
(check-expect (project-list-ordered-by-id-num BST2) (list "Project 2" "Project 3" "Project 1" "Project 4" "Project 5"))

;;8)

(define (new-project BST project-id name title category)
  (cond
    [(false? BST) (make-projectnode project-id name title empty category false false)]
    [(= (projectnode-project-id-num BST) project-id) BST]
    [(< project-id (projectnode-project-id-num BST))
     (make-projectnode (projectnode-project-id-num BST)
                       (projectnode-advisor BST)
                       (projectnode-title BST)
                       (projectnode-students BST)
                       (projectnode-category BST)
                       (new-project (projectnode-left BST) project-id name title category) 
                       (projectnode-right BST))]
    [(projectnode? BST)
     (make-projectnode (projectnode-project-id-num BST)
                       (projectnode-advisor BST)
                       (projectnode-title BST)
                       (projectnode-students BST)
                       (projectnode-category BST)
                       (projectnode-left BST)
                       (new-project (projectnode-right BST) project-id name title category))]))

;; check-expects:
(check-expect 
 (new-project WPI-BST 45.567 "Professor Zhang" "Project H" "IQP")
 (make-projectnode 42.205 "Professor Waltz" "Project A" L1 "IQP"
                   (make-projectnode 39.173 "Professor Jameson" "Project B" L2 "empty"
                                     (make-projectnode 36.114 "Professor Jones" "Project D" L4 "ISP" false false)
                                     (make-projectnode 40.198 "Professor Jung" "Project E" L5 "IQP" false false))
                   (make-projectnode 47.386 "Professor Igor" "Project C" L3 "MQP"
                                     (make-projectnode 45.567 "Professor Zhang" "Project H" empty "IQP" false false)
                                     (make-projectnode 51.232 "Professor Benidict" "Project F" L6 "ISP"
                                                       (make-projectnode 51.118 "Professor Smith" "Project G" L7 "MQP" false false) false))))

(check-expect (new-project WPI-BST 42.205 "Professor Waltz" "Duplicate Project" "IQP") WPI-BST)

;; Bonus

;; student-advisor-project: BST String String -> String
;; The function consumes a binary search tree, a student's email, and an advisor's name, and returns the project title they share or "No Shared Project"

(define (student-advisor-project BST email name)
  (cond
    [(false? BST) "No Shared Project"]  
    [(and (student-in-cur-list? email (projectnode-students BST))  (string=? (projectnode-advisor BST) name)) (projectnode-title BST)]  
    [(projectnode? BST)
     (if (not (string=? (student-advisor-project (projectnode-left BST) email name) "No Shared Project"))
         (student-advisor-project (projectnode-left BST) email name)  
         (student-advisor-project (projectnode-right BST) email name))]))  

(check-expect (student-advisor-project WPI-BST "jimac@wpi.edu" "Professor Jesus") "No Shared Project")
(check-expect (student-advisor-project WPI-BST "krmitchell@wpi.edu" "Professor Jung") "Project E")
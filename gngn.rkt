
;;Data format: Name, Mother, Father, Date of birth, Date of death.
;;An empty list means Unknown.

;;Maternal branch
(define Mb
'(((Mary Blake) ((Ana Ali) (Theo Blake)) ((17 9 2022) ()))
((Ana Ali) ((Ada West) (Md Ali)) ((4 10 1995) ()))
((Theo Blake) ((Mary Jones) (Tom Blake)) ((9 5 1997) ()))
((Greta Blake) ((Mary Jones) (Tom Blake)) ((16 3 1999) ()))
((Mary Jones) (() ())((12 5 1967) (19 5 2024)))
((Tom Blake) (() ()) ((17 1 1964) ()))
((Ada West) (() ()) ((22 8 1973) ()))
((Md Ali) (() ()) ((14 2 1972) (2 5 2023)))
((Ned Bloom) (() ()) ((23 04 2001)()))
((John Bloom) ((Greta Blake) (Ned Bloom)) ((5 12 2023) ()))))

;,Paternal branch
(define Pb
'(((John Smith) ((Jane Doe) (Fred Smith)) ((1 12 1956) (3 3 2021))) 
((Ana Smith) ((Jane Doe) (Fred Smith)) ((6 10 1958) ()))
((Jane Doe) ((Eve Talis) (John Doe)) ((2 6 1930) (4 12 1992)))
((Fred Smith) ((Lisa Brown) (Tom Smith)) ((17 2 1928) (13 9 2016)))
((Eve Talis) (() ()) ((15 5 1900) (19 7 1978)))
((John Doe) (() ()) ((18 2 1899)(7 7 1970)))
((Lisa Brown) (() ())((31 6 1904) (6 3 1980)))
((Tom Smith) (() ()) ((2 8 1897) (26 11 1987)))
((Alan Doe) ((Eve Talis) (John Doe)) ((8 9 1932) (23 12 2000)))
((Mary Doe) (() (Alan Doe)) ((14 4 1964) ()))))

;;define lst-mb
;;define lst-pb
;;define lst-all


;;NOELIA'S HELPER FUNCTION
;These are my helper functions to obtain the data I will be using on the functions to reduce the use of car,cdr etc. This will help keep he code easier to understand as it will specify what information from the list the function is using

(define (full-name person)(car person))
(define (first-name person)(car (car person)))
(define (last-name person)(cadr(car person)))
(define (parents person)(cadr person))
(define (mother person)(car (cadr person)))
(define (father person)(cadr (cadr person)))
(define (dates person)(caddr person))
(define (birth-date person)(car (caddr person)))
(define (birth-month person)(cadr (car (caddr person))))
(define (birth-year person)(caddr (car (caddr person))))
(define (death-date person)(cadr (caddr person)))
(define (death-year person)
  (if (null? (cadr (caddr person)))
      #f
      (caddr (cadr (caddr person)))))

;this is a built-in function that will give me the current year so I can use it when calculating the current age of the family member (roughly)
(require racket/date)
(define (current-year)
  (date-year (current-date)))

;(current-year)

;this will show either true or false if the selected person on the list is alive (true) or dead (false). This function will then be used to obtain the oldest living member by removing the dead ones.
(define (alive? person)
      (null? (death-date person)))

;this will show the age of the person (if alive) otherwise, it will display false. This function will be used to calculate who is oldest out of the living members.
(define (current-age person)
  (if (alive? person)
      (- (current-year) (birth-year person))
      #f))

;(current-age (list-ref Pb 9))


;this will show the age when a person from the list died (if still alive, it will return flse). If the person is dead, then it will deduct the year of their death from their birth date to obtain the age at death
(define (age-at-death person)
  (if (null? (death-date person))
      #f ;they are alive
      (- (death-year person) (birth-year person))))

;this will create a list with only the dead family members by filtering out the people who do not have a death date.
(define (dead-people lst)
  (filter (λ (person) (not (null? (death-date person)))) lst))

;;(dead-people Pb)

;;EXTRA FUNCTION TO REMOVE DUPLICATE FROM ALESSIA
;function to remove duplicate from list as extra for personal preferences
;; it first check if list is empty (base case) so in that case it would stop the recursion
;;then the recursive case takes the first element and the rest os the list
;;the function calls a new filtered list checking if element x is not equal to any other element on the ist (cdr lst) and it moves on recursively
;;it then concatenate all the results creating a new list with no duplicate
(define (no-duplicate lst)
  (if (null? lst) () 
      (cons (car lst) (no-duplicate (filter (λ (x) (not (equal? x (car lst)))) (cdr lst)))))) 

;TEST the no-duplcitae function on Mb only
;(no-duplicate (map cadr Mb)) 

;;A1 = it should return a list of all the parents in the branch
(newline)
;;this function called parents takes 2 lists as input (becuase it needs to return a list of all the parents in the branch)
;;OPTIONAL: it avoid repitition using no-duplicate function created (and explained) above
;;it first filters all the elements in the list where there are NOT empty parentheses which means that there is not parents data shown, so we do not take them into consideration 
;;then using map we apply the filter rule to every second element of the list (cadr = first element of the rest of the list (cdr)))
;;it then returns a list with only the parents of the branch 

(define (parenti lst)
  (no-duplicate 
   (filter (λ (parent) (not (equal? parent '(() ())))) 
     (map cadr lst)))) 

(display "list of all the parents on maternal branch excluding the duplicate")
(newline)
(parenti Mb)


;; A2 = should return all living members of the branch
(newline)
;;to check if members are alive we first filter all these elements where on the second element of the last element of the list (cadr + caddr) is present and empty parenthese meaning no date fo death shown, so the member would be alive
;;then with the usage of map we apply the same filter function to each element of the list so it will return a list of all the members without a date of death shown

(define (living-members lst)
   (map car (filter (λ (dod) (equal? (cadr (caddr dod)) '())) lst)))


;TEST function
;'"the living members of Mb are'"
;(living-memb Mb)

(display "list of all the names of alive members on maternal branch")
(newline)
(living-members Mb)
  
;; A3 = return a list of the current age of all living members
(newline)
;;it is defined first a function 'age' that takes a list and a year as input and it will return a pair of names + age
;;it first filter all the alive members by checking if there is an empty parentheses (which means no date of death shown) and taking only the year which is the 3rd element of the first parentheses of the last element (cadr(caddr year))
;;then it applies the same filter troughout the list using map, and it extracs the name (car name) and combines it with the substruction of the current given year to the year when the member was born
;;to make it easier then this function is calld in current-age-p and applied to both lists 

(define (age lst year)
  (map (λ (name) (list (car name) (- year (caddar (caddr name)))))  
       (filter (λ (year) (equal? (cadr (caddr year)) '())) lst))) 

;;TEST age function on MB list
;;(age Mb 2025)

;function to combine the age function above to both maternal and paternal branch
(define (current-age-p lst1 lst2 year)
  (append (age lst1 year) (age lst2 year)))

(display "list of all the alive people from both maternal and paternal branches and their respective age")
(newline)
(current-age-p Mb Pb 2025)

 ;; A4 = should return a list of all members who have birthdats in the same month as yours
(newline)

;;it is defined function birthday that takes 2 lists and a month as input
;;it first filter in the list the month by taking the 2nd number from the first parentheses of the last element (cadar (caddr mon)
;;still filtering, it checks if that month equal to the given month as input
;; it then returns a mapped list menaing that the same filter is applied to every single element and it aslo returns a pairs name + month

(define (same-birthday-month lst1 lst2 month)
  (append
   (map car (filter (λ (mon) (equal? (cadar (caddr mon)) month)) lst1))
   (map car (filter (λ (mon) (equal? (cadar (caddr mon)) month)) lst2))))

;;TESTING different months
;;(display "list of all members who were born in November")
;;(newline)
;;(same-birthday-month Mb Pb 11)
;;(display "list of all members who were born in May (")
;;(newline)
;;(same-birthday-month Mb Pb 5)

(display "list of all members who were born in January")
(newline)
(same-birthday-month Mb Pb 1)

;; A5 = should return a sorted list of all members in maternal branch by last name
(newline)
;;sort-by-last function defined and it takes a list as input
;;we use sort that takes two arguments (n as name and s as surname) and compare them by extracting them singularly using (cadr (car n/s))
;;since sorts works on strings we then convert them into string and then compare it using string<? that checks if the string is alphabetically before another one

(define (sort-by-last lst)
  (sort lst (λ (n s) (string<? (symbol->string (cadr (car n))) 
                    (symbol->string (cadr (car s)))))))
  
(display "sorted list of all members by their surnames")
(newline)
(sort-by-last Mb)

;; A6 = should return a new family tree changing the name of any member from John to Jjuan  
(newline)
;;the main aim is to check recursively the list by changing the old-name (John) into Juan. Base case to exit the recursion = empty list
;;the function change-name takes a list, a new name and an old name as input
;;if the first element equal to old-name (John) then it will be replaced with the new-name (Juan)
;;it then recusrively check the rest of the list (cdr lst)
;; becuase it is a sublist we then check if the first element is a list? then it will apply the fuunction to the first element living the 'surname' unchanged

(define (change-name lst old-name new-name)
  (cond ((null? lst) '())
    ((equal? (car lst) old-name)
     (cons new-name (change-name (cdr lst) old-name new-name)))  
    ((list? (car lst)) (cons (change-name (car lst) old-name new-name) (change-name (cdr lst) old-name new-name)))
    (else (cons (car lst) (change-name (cdr lst) old-name new-name))))) 


;;TEST same function on paternal branch with another name applied on parents too 
;;(change-name Pb 'Eve 'Alessia)

(display "new list with all the names changed from John to Juan")
(change-name Mb 'John 'Juan)
(newline)

;;
;B1
; This function will retrieve a list of people who have parents, either a mother or a father.
(define (children lst)
  (filter (λ (person) 
                (or (not (null? (mother person)))  
                    (not (null? (father person)))))
              lst))

;B2
; This function will retrieve the oldest living member 
(define (oldest-living-member lst)
  (foldl (λ (person oldest)
           (if (and (alive? person)
                    (or (not oldest) (> (current-age person) (current-age oldest))))
               person 
               oldest)) #f lst))



;B3
;this function will use the data from the dead-people function (helper function above) to add up all the ages at death and divide them by the amount of people who are on the dead-people list.
(define (average-age-on-death lst)
  (if (null? dead-people)
      0
      (floor (/ (foldl + 0 (map age-at-death (dead-people lst))) (length (dead-people lst)))))) 

;(display "Edades al morir")(map age-at-death Pb)

;B4
;this function will retrieve the people from the list who share the same birth month by filtering out the people who meet the condition and will return a list
(define (birthday-month-same lst month)
  (filter (λ (person) (= (birth-month person) month)) lst))

;B5
; This function sorts the list of people alphabetically by their first name. It uses the built-in 'sort' function and a lambda function that compares the first names of two people to determine their order.
(define (sort-by-first lst)
  (sort lst (λ (x y) (symbol<? (first-name x) (first-name y)))))


;B6
; This function changes a given first name to a new name in the list. It uses map to go through each person and checks if their first name matches the old name. If it does, the function updates the name while keeping everything else the same.
(define (change-name-to-Maria lst old-name new-name)
  (map (λ (person)
         (if (equal? (first-name person) old-name)
             (list (list new-name (last-name person))(parents person)(dates person))
             person))
       lst))
;;

;C1
(define (lst-mb mb)
  mb)

;C2
(define (lst-pb pb)
  pb)

;C3
(define (append-lst list1 list2)
        (if (null? list1) list2
            (cons (car list1) (append-lst (cdr list1) list2))))

(define (lst-all mb pb)
  (append-lst mb pb))






(display "The children are: ") (map car(children Pb))
(display "The oldest living person is: ")(car(oldest-living-member Pb))
(display "The average living age is: ")(average-age-on-death Pb)
(display "People born in June: ")(map car(birthday-month-same Pb 6))
(display "Sorted list by first name: ")(newline)(map car(sort-by-first Pb))
(display "Change name from Mary to Maria: ")(newline)(change-name-to-Maria Pb 'Mary 'Maria)
(display "All the names in the maternal list: ")(newline)(map car(lst-mb Mb))
(display "All the names in the paternal list: ")(newline)(map car(lst-pb Pb))
(display "All the names in both branches: ")(newline)(map car(lst-all Mb Pb))

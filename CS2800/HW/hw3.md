```lisp
#|

CS 2800 Homework 3 Solution - Fall 2018

This homework is done in groups. More elaborate instructions are 
posted on the course website:

 * One group member will create a group in BlackBoard.
 
 * Other group members then join the group.
 
 * Homework is submitted once. Therefore make sure the person
   submitting actually does so. In previous terms when everyone needed
   to submit we regularly had one person forget but the other submissions
   meant the team did not get a zero. Now if you forget, your team gets 0.
   - It wouldn't be a bad idea for groups to email confirmation messages
     to each other to reduce anxiety.

 * Submit the homework file (this file) on Blackboard.  Do not rename 
   this file.  There will be a 10 point penalty for this.

 * You must list the names of ALL group members below, using the given
   format. This way we can confirm group membership with the BB groups.
   If you fail to follow these instructions, it costs us time and
   it will cost you points, so please read carefully.

The format should be: FirstName1 LastName1, FirstName2 LastName2, ...
There will be a 10 pt penalty if your names do not follow this format.

Names of ALL group members: Kevin Zhang, Jemin Park

* Later in the term if you want to change groups, the person who created
  the group should stay in the group. Other people can leave and create 
  other groups or change memberships (the Axel Rose membership clause). 
  We will post additional instructions about this later.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

For this homework you will need to use ACL2s.

Technical instructions:

- open this file in ACL2s as hw03.lisp

- make sure you are in BEGINNER mode. This is essential! Note that you can
  only change the mode when the session is not running, so set the correct
  mode before starting the session.

- insert your solutions into this file where indicated (usually as "...")

- only add to the file. Do not remove or comment out anything pre-existing unless
  we tell you to.

- make sure the entire file is accepted by ACL2s. In particular, there must
  be no "..." left in the code. If you don't finish all problems, comment
  the unfinished ones out. Comments should also be used for any English
  text that you may add. This file already contains many comments, so you
  can see what the syntax is.

- when done, save your file and submit it as hw03.lisp

- avoid submitting the session file (which shows your interaction with the
  theorem prover). This is not part of your solution. Only submit the lisp
  file.

Instructions for programming problems:

For each function definition, you must provide both contracts and a body.

You must also ALWAYS supply your own tests. This is in addition to the
tests sometimes provided. Make sure you produce sufficiently many new test
cases. This means: cover at least the possible scenarios according to the
data definitions of the involved types. For example, a function taking two
lists should have at least 4 tests: all combinations of each list being
empty and non-empty.

Beyond that, the number of tests should reflect the difficulty of the
function. For very simple ones, the above coverage of the data definition
cases may be sufficient. For complex functions with numerical output, you
want to test whether it produces the correct output on a reasonable
number of inputs.

Use good judgment. For unreasonably few test cases we will deduct points.

We will use ACL2s' check= function for tests. This is a two-argument
function that rejects two inputs that do not evaluate equal. You can think
of check= roughly as defined like this:

(defunc check= (x y)
  :input-contract (equal x y)
  :output-contract (equal (check= x y) t)
  t)

That is, check= only accepts two inputs with equal value. For such inputs, t
(or "pass") is returned. For other inputs, you get an error. If any check=
test in your file does not pass, your file will be rejected.

We also learned about test? in class.  This generally takes the form:
(test? (implies <Data expected> <True statement based on expected data>))

For example, one might write:
(test? (implies (and (listp x)(listp y))(equal (+ (len x)(len y))(len (app x y)))))

|#


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Contract Fulfillment
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|

The following are functions that may or may not be correct. For each function,
list all of the calls that this function makes. For each call decide whether
the contract is satisfied or not. If it is satisfied, then explain why. If it
is not satisfied then explain why not. In other words, show all of the body
contracts. Do the same for the overall function contract.

---------
EXAMPLE
(defunc foo (x)
  :input-contract (listp x)
  :output-contract (natp (foo x))
  (if (endp x)
     0
     (+ 1 (foo (rest x)))))
Body Contract:
(rest x) the input-contract is satisfied because x needs to be a non-empty list
and this is ensured by the if statement and the input contract for foo.

(foo (rest x)) the input-contract is satisfied because (rest x) outputs a list.

(+ 1 (foo (rest x))) the input contract is two natural numbers. 1 is a natural by definition.
The output-contract for foo is natp and thus + has appropriate input.

(endp x) the input-contract requires x to be a list. This is ensured by the input contract
of foo.

(if (endp x)....) the input-contract requires (endp x) to be a boolean.  endp returns a boolean
based on its output contract.

Function Contract:

(+ 1 (foo (rest x))) If the recursive call obeys the contract, then the returned value must be a nat
since the inputs are naturals.

0 Since 0 is an natural number, the output-contract holds.

Thus the overal function contract is satisfied.
----------
|#

;; since the following functions may have errors in their contracts, we turn ACL2s's
;; checking off temporarily
:program
(acl2s-defaults :set testing-enabled nil)

:program


;; 1. The following function supposedly computes the modulo of an integer by a nat.
(defunc mod (x y)
  :input-contract (and (integerp x) (natp y) (not (equal y 0)))
  :output-contract (natp (mod x y))
  (cond ((>= x y) (mod (- x y) y))
        ((>= x 0) x)
        (t (mod (+ x y) y))))

#|
Body Contract:
(>= x y) the input-contract holds because x is an integer, and y is a nat, as ensured by the input contract for mod

(- x y) holds for same reason as (>= x y). Outputs an integer

(mod (- x y) y) the input-contract holds because (- x y) produce an integer, and y is unchanged (and it satisfies the input-contract for mod originally) 

(>= x 0) the input-contract holds because x is an integer, and 0 is an integer

(+ x y) holds for same reason as (- x y). Outputs an integer

(mod (+ x y) y) the input-contract holds because (+ x y) is an integer, and y is unchanged. 

Function Contract:

(mod (- x y) y) holds the output-contract because it is the same function (and therefore must have the same output-contract)

x does hold the output-contract, because even though x is an integer, it must be >= 0, which is the definition of nat.

(mod (+ x y) y) hold the output-contract for same reasons as (mod (- x y) y).

Thus the overal function contract is satisfied.

2. The following function supposedly computes the magnitude of a rational for
scientific notation.....or it used to before I changed the question.  Now
it sometimes does (don't concern yourself too much with what the function does)
|#


(defunc mag (x)
  :input-contract (and (rationalp x) (>= x 0))
  :output-contract (natp (mag x))
  (cond ((equal x 0) 0)
        ((>= x 10) (numerator x))
        ((< x 1) (- (mag (* x 10)) 1))
        (t 0)))

#|
Body Contract:
(equal x 0) equal takes Any, so it is satisfied

(>= x 10) the input-contract is satisfied because x is a rational number from the input-contract of mag.

(numerator x) input-contract is satisfied becasue x is a rational number.

(< x 1) input satisfied becasuse x is rational.

(* x 10) input satisfied because x is rational.

(mag (* x 10)) input satisfied, because x is rational, and 10 times of x >= 0. 

(- (mag (* x 1)) 1) input satisfied, becasue mag returns a nat.

Function Contract:
0 satisfies the output contract because 0 is a nat.

(numerator x) satisfies output contract because numerator returns a nat.

(- (mag (* x 10)) 1) does not satisfy output contract because (mag (* x 1)) returns a nat, which may be 0, and (- (mag (* x 10)) 1) may not be a nat.

0 for same reasons as 0.

Thus, the overall function contract is not satisfied.

3. The following function does something mysterious.
|#

(defunc ack (m n)
  :input-contract (and (natp m) (natp n))
  :output-contract (natp (ack m n))
  (cond ((equal m 0) (+ n m))
        ((equal n 0) (ack (- m 1) 0))
        (t           (ack (- m 1) (ack m (- n 1))))))


#|
Body Contract:
(equal m 0) equal takes any, so it is satisfied

(+ n m) both n and m are nats, so the input-contract is satisfied.

(equal n 0) same reason as (equal m 0)

(- m 1) m is a nat, so input is okay

(ack (- m 1) 0) the input contract is satisfied, because m is a nat > 0 (from first cond condition), so (- m 1) would be another nat.

(- n 1) n is a nat, so input is okay

(ack m (- n 1)) the input contract is satisfied, because n is a nat > 0 (from second cond condition), so (- n 1) would be another nat). m is a nat.

(ack (- m 1) (ack m (- n 1))) the input contract is satisfied, because (- m 1) returns a nat, and (ack m (- n 1)) returns a nat.

Function Contract:
(+ n m) is a nat since both n and m are nats.

(ack (- m 1) 0) returns a nat, assuming the ack output-contract holds.

(ack (- m 1) (ack m (- n 1))) returns a nat, assuming the ack output-contract holds.

Thus, the overall function contract is satisfied.

|#
 
;; Turn proper testing back on
:logic
(acl2s-defaults :set testing-enabled t)

;; Assume by "Def of lor" that each element is a rational
;; and a lor is (cons rational lor) | nil
;; A similar claim can be made about a lon
(defdata lor (listof rational))
(defdata lon (listof nat))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; GIVEN
;; del: Any x List -> List
;; (del e l) takes an element e and a list l
;; and removes the FIRST occurance of e from l
;; If e is not in l then l is returned.
(defunc del (e l)
  :input-contract (listp l)
  :output-contract (listp (del e l))
  (if (endp l)
    l
    (if (equal e (first l))
      (rest l)
      (cons (first l) (del e (rest l))))))

(sig del (all (listof :b)) => (listof :b))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; GIVEN
;; perm: List x List -> Boolean
;; (perm l1 l2) takes two lists (l1 and l2) and
;; returns true if and only if l1 and l2 have
;; the same elements (and the same number of each)
;; Essentially: is l2 a reordering of l1?
(defunc perm (l1 l2)
  :input-contract (and (listp l1)(listp l2))
  :output-contract (booleanp (perm l1 l2))
  (if (equal l1 l2)
    t
    (if (endp l1)
      nil
      (and (in (first l1) l2)
           (perm (rest l1) (del (first l1) l2))))))

;; GIVEN
;; intersect: list x list -> list
;; (intersect l1 l2) returns a list containing every element 
;; that is in both lists l1 and l2.
(defunc intersect (l1 l2)
    :input-contract (and (listp l1) (listp l2))
    :output-contract (listp (intersect l1 l2))
    (cond ((endp l1) l1)
          ((in (first l1) l2) 
           (cons (first l1)
                 (intersect (rest l1) 
                            (del (first l1) l2))))
          (t (intersect (rest l1) l2))))


 ;;;;;;;;;;;;;;;;;;;;;;;;;;;
 ;; For sections II and III, DEFINE the functions indicated (typically denoted with "..."  
 ;; Make sure to write appropriate tests for each function that you define.


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Section II: Calculating Your Grade
;;
;; Soon I will be asking you to write a program which determines
;; your mark in the course (given that BB stinks for handling
;; dropped grades). Note, this also means that at the end of term, if
;; you ask us what your grade is or why blackboard is saying X, we will
;; point you to this assignment.
;; However, first I want to define how we can store these data 
;; (FYI: Yes THESE data. Data is the plural of datum)
;;
;; We will split grades up by category (exam, assignment, or quiz)
;; Each category has a name, its weight (percentage of total grade),
;; number of grades counted (to exclude dropped grades). the max score for 
;; each assignment, and a list of the grades in that category. A full gradebook
;; is composed of all three grade categories.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; 1. DEFINE an enumerated datatype for category names
;; (exam, assignment or quiz).
(defdata category-name (enum '(assignment quiz exam)))

(check= (category-namep 'assignment) t)
(check= (category-namep 'quiz) t)
(check= (category-namep 'tv) nil)

;; 2. DEFINE the possible weights as the values 
;; from 0 to 1, inclusive.
(defdata category-weight (range rational (0 <= _ <= 1)))

(check= (category-weightp 66/100) t)
(check= (category-weightp 2/3) t)
(check= (category-weightp 66) nil)
(check= (category-weightp 1) t)


;; NOTE: These constants will be useful later in the program
(defconst *num-assignments* 10)
(defconst *num-exams* 2)
(defconst *num-quizzes* 20)

(defconst *pct-assignments* 1/5)
(defconst *pct-quizzes* 1/5)
(defconst *pct-exams* 3/5)

(defconst *assignments-max* 100)
(defconst *exams-max* 100)
(defconst *quizzes-max* 24)


;; this line is for ACL2s
(sig intersect ((listof :b) (listof :b)) => (listof :b))


;; Also DEFINE the valid values for quizzes, exams, and assignments
;; using the constants above and a range.
;; THEN make a data definition that is the union of these sets of values
;; to help us deal with lists of values (a logr).  Scores are rationals
;; to handle partial credit.
(defdata quiz-range (range rational (0 <= _ <= *quizzes-max*)))
(defdata loqr (listof quiz-range)) 
(defdata assig-range (range rational (0 <= _ <= *assignments-max*)))
(defdata loar (listof assig-range))
(defdata exam-range (range rational (0 <= _ <= *exams-max*)))
(defdata loer (listof exam-range))
(defdata logr (listof (oneof exam-range quiz-range assig-range)))


(check= (loerp '(80 20 34)) t)
(check= (loarp '(80 20 34)) t)
(check= (loqrp '(80 20 34)) nil)
(check= (loqrp '(24 0 12 10 17 23)) t)
(check= (logrp '(24 0 12 10 17 23)) t)
(check= (logrp '(24 0 12 10 17 25)) t) ;; It's a list of scores but NOT a list of quiz grades


;; 3. DEFINE a grade category as a record containing a name, weight, num-counted,
;; and grades. Num-counted should be a positive natural, 
;; and grades should be a list of grades (logr). 
;; More concretely, the tests below should pass.
(defdata grade-category (record (name . category-name)
                                (weight . category-weight)
                                (num-counted . pos)
                                (grades . logr)))


(test? (implies (grade-categoryp c) (category-namep (grade-category-name c))))
(test? (implies (grade-categoryp c) (category-weightp (grade-category-weight c))))
(test? (implies (grade-categoryp c) (posp (grade-category-num-counted c))))
(test? (implies (grade-categoryp c) (logrp (grade-category-grades c))))

;; 4. DEFINE a gradebook as a list containing exactly three grade categories: assignments,
;; quizes, and exams.
(defdata gradebook (list grade-category
                          grade-category 
                          grade-category))

(test? (implies (gradebookp g) (equal 3 (len g))))
(test? (implies (gradebookp g) (grade-categoryp (first g))))
(test? (implies (gradebookp g) (grade-categoryp (second g))))
(test? (implies (gradebookp g) (grade-categoryp (third g))))

;; 5. DEFINE a datatype for letter grades (A, A-, B+, B,.... F)
(defdata lettergrade (enum '(A A- B+ B B- C+ C C- D+ D D- F)))

(check= (lettergradep 'A) t)
(check= (lettergradep 'F) t)
(check= (lettergradep 'E) nil)
(check= (lettergradep 'A+) nil)
(check= (lettergradep 'D+) t)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; To avoid having to keep typing a long list of grade records, it is handy
; to define a global constant which can then be used for many check= tests.
; You should put all of the grades of your team in gradebooks. You can make
; a gradebook for each group member and we suggest your gradebook should
; contain your actual grades. ie Your check= tests should use your own 
; gradebook. Here is a modifiable example
(defconst *cs2800-a1* (grade-category 'assignment
                                      *pct-assignments* *num-assignments*
                                      '(90 80 78 82 91 85 71 64 83 85 81)))
(check= (loarp (grade-category-grades *cs2800-a1*)) t)

(defconst *cs2800-q1* (grade-category 'quiz
                                      *pct-quizzes* *num-quizzes*
                                      '(18 18 18 18 24 24 24 18 18 18 24
                                        0  0  18 0  24 6  18 0  24 18 12)))
(check= (loqrp (grade-category-grades *cs2800-q1*)) t)

(defconst *cs2800-e1* (grade-category 'exam 
                                      *pct-exams* *num-exams*
                                      '(74 78)))
(check= (loerp (grade-category-grades *cs2800-e1*)) t)

(defconst *cs2800* (list *cs2800-a1* *cs2800-q1* *cs2800-e1*))
        
        

;; GIVEN
;; sum-l: lor -> rational
;; (sum-l lr) takes a list of rationals (lr) and returns the sum of all
;; elements in the list.  If lr is empty, then return 0.
(defunc sum-l (lr)
  :input-contract (lorp lr)
  :output-contract (rationalp (sum-l lr))
  (if (endp lr)
    0
    (+ (first lr)(sum-l (rest lr)))))


;; GIVEN
;; min-l: lor (non-empty) -> rational
;; (min-l lr) takes a non-empty list of rationals (lr) and finds
;; the smallest value in that list.
(defunc min-l (lr)
  :input-contract (and (lorp lr)(consp lr))
  :output-contract (rationalp (min-l lr))
  (if (endp (rest lr))
    (first lr)
    (let ((mr (min-l (rest lr))))
    (if (<= (first lr) mr)
      (first lr)
      mr))))

;; 6. DEFINE
;; drop-n-lowest : Lor x Nat -> Lor
;; removes the n lowest rationals from the list, up to the length of the list
(defunc drop-n-lowest (l n)
    :input-contract (and (lorp l) (natp n))
    :output-contract (lorp l)
    (if (or (endp l) (equal n 0))
        l
        (drop-n-lowest (del (min-l l) l) (- n 1))))

;; Write sufficient tests
(test? (implies (and (lorp l) (posp n))
                (< (drop-n-lowest l n)) (len l)))
(test? (implies (and (lorp l) (rationalp e) (posp n) (not (in e (del e l))) (in e l) (equal e (min-l l)))
                (not (in e (drop-n-lowest l n)))))
(test? (implies (and (lorp l) (natp n) (< n (len l)))
                (equal (- (len l) (len (drop-n-lowest l n)))
                       n)))
(check= (drop-n-lowest '(1 2 3) 1) '(2 3))
(check= (drop-n-lowest '(1 2 3) 4) '())

(defthm phi_logr (implies (logrp l)(lorp l)))

;; Student NOTE: The code needs this to run for some reason. Otherwise, we get 
;; an error involving induction proof 
:program 

;; 7. DEFINE
;; get-counted-grades : grade-category -> lorp
;; returns a list containing all of the grades that should be counted from this
;; category
(defunc get-counted-grades (c)
    :input-contract (grade-categoryp c)
    :output-contract (lorp (get-counted-grades c))
    (let* ((num-counted (grade-category-num-counted c))
           (grades (grade-category-grades c))
           (grades-len (len grades)))
          (if (> grades-len num-counted)
              (drop-n-lowest grades (- grades-len num-counted))
              grades)))

;; Write sufficient tests
(check= (get-counted-grades *cs2800-a1*) '(90 80 78 82 91 85 71 83 85 81))
(check= (get-counted-grades *cs2800-e1*) '(74 78))
(test? (implies (and (logrp l) (category-name n) (posp k) (> (len l) k) (category-weightp w))
                (equal (len (get-counted-grades (grade-category n w k l))) k)))
(test? (implies (and (logrp l) (category-name n) (posp k) (<= (len l) k) (category-weightp w))
                (equal l (get-counted-grades (grade-category n w k l)))))

;; 8. DEFINE
;; get-category-pts : grade-category -> rational
;; returns the number of points you receive (for your final grade) based on the category.
;; overall grade in the given category as a rational 
;; in the range 0 <= r <= 1
(defunc get-category-pts (c)
    :input-contract (grade-categoryp c)
    :output-contract (and (category-weightp (get-category-pts c)))
    (let* ((weight (grade-category-weight c))
           (grades (get-counted-grades c))
           (grades-sum (sum-l grades))
           (name (grade-category-name c))
           (num-counted (grade-category-num-counted c)))
          (if (equal name 'quiz)
              (* (/ (* 100/24 grades-sum) num-counted) (/ weight 100))
              (* (/ grades-sum num-counted) (/ weight 100)))))

;; Write sufficient tests
(check= (get-category-pts *cs2800-a1*) 1652/10000)
(check= (get-category-pts *cs2800-q1*) 1425/10000)
(check= (get-category-pts *cs2800-e1*) 456/1000)
(check= (get-category-pts (grade-category 'exam 1/5 20 '())) 0)
(test? (implies (and (loqrp l) (category-weightp w) (posp k) (equal (len l) k))
                (equal (* (/ (sum-l l) k) (/ w 100))
                       (get-category-pts (grade-category 'quiz w k l)))))

;........make sure you test sufficiently and you define the various letter 
;; grades

;; don't ask why this is necessary here; it's complicated
:program

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; DEFINE
;; get-pct-final: gradebook -> rational
;; (get-pct-final calculates the pct grade for students
;; This can be part of get-grade but I pulled it out for debugging
;; purposes
(defunc get-pct-final (gb)
  :input-contract (gradebookp gb)
  :output-contract (rationalp (get-pct-final gb))
  (* (+ (get-category-pts (first gb))
     	(+ (get-category-pts (second gb))
     	   (get-category-pts (third gb)))) 100))

(check= (get-pct-final *cs-2800*) 7637/100)
(test? (implies (gradebookp gb) (<= (get-pct-final gb) 100)))
(test? (implies (gradebookp gb) (>= (get-pct-final gb) 0)))

:logic

;; 9. DEFINE
;; get-grade : gradebook -> lettergrade
;; returns the overall grade in class as a letter grade. For the purposes of this
;; assignment, assume that grades are assigned to the following ranges:
;; F: 0 <= _ < 50
;; D-: 50 <= _ < 60
;; D: 60 <= _ < 65
;; D+: 65 <= _ < 70
;; C-: 70 <= _ < 74
;; C: 74 <= _ < 77
;; C+: 77 <= _ < 80
;; B-: 80 <= _ < 84
;; B: 84 <= _ < 87
;; B+: 87 <= _ < 90
;; A-: 90 <= _ < 94
;; A: 94 <= _ <= 100
;; Please define constansts for the letter grade ranges
(defunc get-grade (gb)
    :input-contract (gradebookp gb)
    :output-contract (lettergradep (get-grade gb))
    (let ((final (get-pct-final gb)))
         (cond 
             ((and (<= 94 final) (<= final 100)) 'A)
             ((and (<= 90 final) (< final 94)) 'A-)
             ((and (<= 87 final) (< final 90)) 'B+)
             ((and (<= 84 final) (< final 87)) 'B)
             ((and (<= 80 final) (< final 84)) 'B-)
             ((and (<= 77 final) (< final 80)) 'C+)
             ((and (<= 74 final) (< final 77)) 'C)
             ((and (<= 70 final) (< final 74)) 'C-)
             ((and (<= 65 final) (< final 70)) 'D+)
             ((and (<= 60 final) (< final 65)) 'D)
             ((and (<= 50 final) (< final 60)) 'D-)
             (t 'F))))

(test? (implies (and (gradebookp gb) (< (get-pct-final gb) 50))
                (equal (get-grade gb) 'F)))
(test? (implies (and (gradebookp gb) (< (get-pct-final gb) 60) (<= 50 (get-pct-final gb)))
                (equal (get-grade gb) 'D-)))
(test? (implies (and (gradebookp gb) (< (get-pct-final gb) 65) (<= 60 (get-pct-final gb)))
                (equal (get-grade gb) 'D)))
(test? (implies (and (gradebookp gb) (<= (get-pct-final gb) 100) (<= 94 (get-pct-final gb)))
                (equal (get-grade gb) 'A)))
(test? (implies (and (gradebookp gb) (equal (get-pct-final gb) 84))
                (equal (get-grade gb) 'B)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Section III: GCD
;; OK.  I'm going to pester my CS 1802 students with these sorts of
;; problems so I thought it only fair for you to go through the same
;; thing.  Remember that the Greatest Common Divisor (GCD) is largest
;; natural number that positively divides two natural numbers. It can
;; be calculated by factoring two numbers and multiplying together
;; all common factors.  Thus (GCD 24 16) is 8 because
;; (factors 24) can be UNIQUELY written as a product of primes
;; (2 * 2 * 2 * 3) while (factors 16) is (2 * 2 * 2 * 2).  They
;; have three 2s in common.
;;....what if we took this a step further and calculated the GCD
;; for a LIST of positive integers (function gcd-list).  
;; The returned value would be
;; the largest number that divides all numbers in the list
;; You will be responsible for writing ALL functions and tests (except
;; the ones below which constrain the function name and results to
;; make grading easier. :) ).
;; Feel free to write helper functions.
;;
;; You might want to look at the function "intersect" above.
;;
;; FEEL FREE TO PUT YOUR CODE IN PROGRAM MODE if things get too difficult
;; It's not your job to figure out how to drive ACL2s although
;; if it works in logic mode it's much more impressive (consider
;; this a challenge exercise)

(defdata lop (listof pos))

(defdata factor (range integer (2 <= _)))
;; Ignore
(defthm phi_shrink_factor (implies (and (posp n)(factorp f)(posp (/ n f)))
                                   (< (/ n f) n)))

;; I don't want you spending time getting this admitted into ACL2s.
;; Uncomment if things get too annoying.
;:program 


;; Helper to the primep function
;; primehelper : Pos Pos -> Boolean
;; (primep i n) determines if n is a prime number by dividing all
;; 6k+i numbers from i to sqrt(n) into n to determine if it is a factor. 
(defunc primehelper (i n)
    :input-contract (and (posp i) (posp n))
    :output-contract (booleanp (primehelper i n))
    (if (<= (* i i) n)
        (if (or (equal (mod n i) 0)
                (equal (mod n (+ i 2)) 0))
            nil
            (primehelper (+ 6 i) n))
        t))

;; Using the 6k+i algorithm for primality testing found online
;; primep : Pos -> Boolean
;; (primep p) determines if p is a prime number, where 1 is not a prime
(defunc primep (p)
    :input-contract (posp p)
    :output-contract (booleanp (primep p))
    (cond 
        ((equal p 1) nil)
        ((<= p 3) t) 
        ((equal (mod p 2) 0) nil)
        ((equal (mod p 3) 0) nil)
        (t (primehelper 5 p))))

(check= (primep 10) nil)
(check= (primep 25) nil)
(check= (primep 17) t)
(check= (primep 113) t)
(check= (primep 2) t)
(check= (primep 3) t)

;; nextfactor : Pos Pos -> Prime
;; returns the next prime factor of n in the range [p, infinity)
(defunc nextfactor (p n) 
    :input-contract (and (posp p) (posp n))
    :output-contract (primep (nextfactor p n))
    (if (and (equal (mod n p) 0) (primep p)) 
        p 
        (nextfactor (+ p 1) n)))

(check= (nextfactor 2 20) 2)
(check= (nextfactor 3 20) 5)

            
;; DEFINE
;; factors : Pos -> Lop
;; returns a list containing all PRIME factors of n including (possibly) 
;; n. In all cases, 1 is a factor but is left out for simplicity sake. 
(defunc factors (n)
    :input-contract (posp n)
    :output-contract (lopp (factors n))
    (cond
        ((equal n 1) (list 1))
        ((primep n) (list 1 n))
        (t (let ((factor (nextfactor 2 n)))
             	(cons factor (factors (/ n factor)))))))


;; Note the factors for 12 *could* be listed as 1 2 3 4 6 12 but
;; really 12 is uniquely represented as the product of non-decreasing 
;; prime numbers: 2*2*3.  Thus factors will return '(2 2 3)  
;; 13 is prime so it isn't decomposed into other
;; primes and '(13) is returned. 
;; If you get the same values but in a different order, feel free to 
;; change the tests.
:logic
(check= (factors 1) '(1))
(check= (factors 2)'(1 2))
(check= (factors 4)'(1 2 2))
(check= (factors 6)'(1 2 3))
(check= (factors 12)'(1 2 2 3))
(check= (factors 13)'(1 13))
(check= (factors 100) '(2 2 5 1 5))
(check= (factors 113) '(1 113))


;; mult-l : lor -> rational
;; (mult-l lor) returns the multiplication of all elements in lor
(defunc mult-l (lor)
    :input-contract (lorp lor)
    :output-contract (rationalp (mult-l lor))
    (if (endp lor) 1 (* (first lor) (mult-l (rest lor)))))

(check= (mult-l '()) 1)
(check= (mult-l '(1 2)) 2)
(check= (mult-l (factors 12)) 12)


;; DEFINE
;; gcd-list: lop -> pos
;; (gcd-list lp) determines the greatest common divisor for 
;; the entire list of positive integers lp
(defunc gcd-list (lp)
    :input-contract (lopp lp)
    :output-contract (posp (gcd-list lp))
    (cond 
        ((endp lp) 1)  ;; Should never reach this case
    	((equal (len lp) 1) (first lp))
        (t (mult-l (intersect (factors (first lp)) 
                              (factors (gcd-list (rest lp))))))))


(check= (gcd-list '(4 8 16 24)) 4)
(check= (gcd-list '(4 8 16 24 5)) 1)
(check= (gcd-list '(12 18 36)) 6)
(check= (gcd-list '()) 1)
(check= (gcd-list '(12)) 12)

```




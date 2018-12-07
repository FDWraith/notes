```lisp
#|
CS 2800 Homework 8 - Fall 2018


This homework is to be done in a group of 2-3 students. 

If your group does not already exist:

 * One group member will create a group in BlackBoard
 
 * Other group members then join the group
 
 Submitting:
 
 * Homework is submitted by one group member. Therefore make sure the person
   submitting actually does so. In previous terms when everyone needed
   to submit we regularly had one person forget but the other submissions
   meant the team did not get a zero. Now if you forget, your team gets 0.
   - It wouldn't be a bad idea for group members to send confirmation 
     emails to each other to reduce anxiety.

 * Submit the homework file (this file) on Blackboard.  Do not rename 
   this file.  There will be a 10 point penalty for this.

 * You must list the names of ALL group members below, using the given
   format. This way we can confirm group membership with the BB groups.
   If you fail to follow these instructions, it costs us time and
   it will cost you points, so please read carefully.

Names of ALL group members: Kevin Zhang, Jemin Park

Note: There will be a 10 pt penalty if your names do not follow 
this format.

|#
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
#|
For this homework you may want to use ACL2s to help you.

Technical instructions:

- open this file in ACL2s as hw08.lisp

- make sure you are in BEGINNER mode. This is essential! Note that you can
  only change the mode when the session is not running, so set the correct
  mode before starting the session.

- insert your solutions into this file where indicated (usually as #.....#)

- only add to the file. Do not remove or comment out anything pre-existing
  unless asked to.

- make sure the entire file is accepted by ACL2s. In particular, there must
  be no "..." left in the code. If you don't finish all problems, comment
  the unfinished ones out. Comments should also be used for any English
  text that you may add. This file already contains many comments, so you
  can see what the syntax is.
  
 - Since many of the question are in comment blocks, I'm using the unique tag
 #.....# to identify areas where you need to add your solutions.

- when done, save your file and submit it as hw08.lisp

- avoid submitting the session file (which shows your interaction with the
  theorem prover). This is not part of your solution. Only submit the lisp
  file!

|#
#|
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Measure Functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
A measure function m for function f satisfies the 
following conditions (as discussed in class and in lectures):
1. m has the same arguments and the same input contract as f.
2. m's output contract is (natp (m ...))
3. m is admissible.
4. On every recursive call of f, given the input contract and 
   the conditions that lead to that call, m applied to the arguments in
   the call is less than m applied to the original inputs.
   
Thus when you are asked to prove termination using a measure function, you
need to
a) Write the function (if not provided) which satisfies points 1-3
b) Write proof obligations corresponding to recursive calls in f
c) Prove the proof obligations using equational reasoning or using an approach
   we specify
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
|#

#|
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 Induction Proofs 

We will be looking at inductive proofs (in various forms) for the remainder 
of the term. If you want an example of an inductive proof, look at our proof 
for Gauss' Trick.
phi_sumn: (implies (natp n) (equal (sumn n)(fsumn n)) 
where sumn is the slow way to sum numbers from 0 to n while fsumn is 
n * (n+1) / 2.

We broke that proof into 3 parts based on the induction scheme of nind

phi_sumn1: ~(natp n) => phi_sumn
phi_sumn2: (natp n) /\ (equal n 0) => phi_sumn
phi_sumn3: (natp n) /\ (not (equal n 0)) /\ phi_sumn|((n (- n 1))) => phi_sumn

Notice that phi_sumn1 is the "bad data" or ~IC case which we ignored when doing
equational reasoning.
            
You'll also notice that since the parts imply phi_sumn
you should swap in the ENTIRE phi_sumn conjecture and use
exportation to get your context. Exportation means we just get 
a sequence of ands which imply (sumn n) = (fsumn n) for each of the 
conjectures. For example, phi_sumn2 is 
(natp n) /\ (equal n 0) => ((natp n) => (equal (sumn n)(fsumn n)))
or
(natp n) /\ (equal n 0) /\ (natp n) => (equal (sumn n)(fsumn n))) 

I will use the term proof obligations to refer to conjectures used to prove a
particular conjecture (eg "=> phi_sumn") while the induction scheme can be applied
to any inductive proof (eg "=> phi" where phi is not specified).

For each induction scheme conjecture we add the conditions that 
lead to a particular branch. We also assume that the 
conjecture holds for the next recursive call when a recursive call (or calls) occurs. 
Thus we substitute the arguments of the recursive call into the original conjecture.

For inductive proofs you will always be expected to write proof obligations
or induction schemes, clearly label where these came from, and then prove
the various parts.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
|#

;;==========================================
; Section 1: Induction schemes
;;==========================================


#|

Below you are given the proof obligations generated by
induction schemes. Your job is to: 
1) define functions, using defunc, that give rise to these induction schemes. 
phi|(...) denotes phi under substitution ... . 

2) For each function you write, also write a measure function that could
be used to prove your function works (you can do those proofs for practice
but they aren't required here)

For these functions (f1-f5) you do not need to provide
tests (i.e., no check= forms are required). It is also
a good idea to make these functions as simple as possible.

Example
1. (not (integerp x)) => phi
2. (and (integerp x) (equal x 0)) => phi
3. (and (integerp x) (not (equal x 0))(> x 0))) phi|((x (- x 1))))  => phi
4. (and (integerp x) (not (equal x 0))(not (> x 0)) phi|((x (+ x 1))))  => phi


Recall: phi|((a (rest a)) (b b)) is the same as 
        phi|((a (rest a))).  You can leave off variable parameters 
        that don't change.

;;(defunc f0 (x)
  :input-contract (integerp x)
  :output-contract t
  (cond ((equal x 0)  x)
        ((> x 0)      (f1 (- x 1)))
        (t            (f1 (+ x 1)))))

NOTE: The induction scheme below has been slightly simplified to avoid
the expression being too unruly (eg for obligation 3 you would see 
(not (equal x 1)))

f1)
1. (not (posp x)) => phi
2. (and (posp x) (equal x 1)) => phi
3. (and (posp x) (equal x 2)) => phi
4. (and (posp x) (equal x 3)) => phi
5. (and (posp x) (not (equal x 1)) (not (equal x 2))
(not (equal x 3)) phi|((x (- x 3))))  => phi

|#

(defunc f1 (x)
    :input-contract (posp x)
    :output-contract t
    (cond
        ((equal x 1) x)
        ((equal x 2) x)
        ((equal x 3) x)
        (t (f1 (- x 3)))))

(defunc m-f1 (x)
    :input-contract (posp x)
    :output-contract (natp (m-f1 x))
    x)

#|
f2)
1. (not (and (listp x)(listp y))) => phi
2. (and (listp x)(listp y)(endp x)(endp y)) => phi
3. (and (listp x)(listp y)(endp x)(phi|((y (rest y))))) => phi
4. (and (listp x)(listp y)(not (and (endp x))(endp y))(not (endp x))
        (phi|((x (rest x))(y (cons (first x) y))))) 
   => phi
|#

(defunc f2 (x y)
    :input-contract (and (listp x) (listp y))
    :output-contract t
    (cond 
        ((and (endp x) (endp y)) 0)
        ((endp x)  (f2 x (rest y)))
        (t (f2 (rest x) (cons (first x) y)))))

(defunc m-f2 (x y)
    :input-contract (and (listp x) (listp y))
    :output-contract (natp (m-f2 x y))
    (if (endp x)
        (len y)
        (+ (* 2 (len x)) (len y))))



#|
f3)
1. (not (and (listp x)(natp y))) => phi
2. (and (listp x)(natp y)(endp x)(equal y 0)) => phi
3. (and (listp x)(natp y)(not (and (endp x)(equal y 0)))
        (endp x) phi|((y (- y 1)))) => phi
4. (and (listp x)(natp y)(not (and (endp x)(equal y 0)))(not (endp x))
        (equal y 0) phi|((x (rest x)))) => phi
5. (and (listp x)(natp y)(not (and (endp x)(equal y 0)))(not (endp x))
        (not (equal y 0))  phi|((x (rest x))(y (- y 1))) )
    => phi

|#

(defunc f3 (x y)
    :input-contract (and (listp x) (natp y))
    :output-contract t
    (cond 
        ((and (endp x) (equal y 0)))
        ((endp x)      (f3 x (- y 1)))
        ((equal y 0)   (f3 (rest x) y))
        (t             (f3 (rest x) (- y 1)))))

(defunc m-f3 (x y)
    :input-contract (and (listp x) (natp y))
    :output-contract (natp (m-f3 x y))
    (+ (len x) y))


;; The following functions are not trivial to admit into ACL2s in logic
;; mode.  For f4 and f5, just convince yourself that the terminate and IC=>OC
:program


#|
f4)
1. (not (integerp x)) => phi
2. (and (integerp x) (< x -1)) => phi
4. (and (integerp x) (not (< x -1)) 
        phi|((x (- x 1))) phi|((x (- x 2)))  => phi

|#

(defunc f4 (x)
    :input-contract (integerp x)
    :output-contract t
   	(if (< x -1)
        0
        (+ (f4 (- x 1)) (f4 (- x 2)))))

(defunc m-f4 (x)
    :input-contract (integerp x)
    :output-contract (natp (m-f4 x))
    (if (< x -2)
        0
        (+ x 3)))

#|
f5)
1. (not (and (listp x) (integerp y))) => phi
2. (and (listp x) (integerp y) (endp x) (equal y -1)) => phi
3. (and (listp x) (integerp y) (not (and (endp x) (equal y -1)))
        (endp x)(< y -1) phi|((y (+ y 1)))) => phi 
4. (and (listp x) (integerp y) (not (and (endp x) (equal y -1)))
        (not (and (endp x)(< y -1)))
        (endp x) (phi|((x (cons 1 x)) (y (- y 1))))) => phi 
5. (and (listp x) (integerp y) (not (and (endp x) (equal y -1)))
        (not (and (endp x)(< y -1)))
        (not (endp x)) (phi|((x (rest x))))) => phi 


|#

(defunc f5 (x y)
    :input-contract (and (listp x) (integerp y))
    :output-contract t
    (cond
        ((and (endp x) (equal y -1)) 0) 
        ((and (endp x) (< y -1)) (f5 x (+ y 1)))
        ((endp x) (f5 (cons 1 x) (- y 1)))
        (t (f5 (rest x) y))))

#|
(f5 '(1 2 3) 4) 13
(f5 '(2 3) 4) 12 
(f5 '(3) 4) 11
(f5 '() 4) 10
(f5 '(1) 3) 9
(f5 '() 3) 8
(f5 '(1) 2) 7
(f5 '() 2) 6
(f5 '(1) 1) 5
(f5 '() 1) 4 
(f5 '(1) 0) 3
(f5 '() 0) 2
(f5 '(1) -1) 1
(f5 '() -1) 0
|#

(defunc m-f5 (x y)
    :input-contract (and (listp x) (integerp y))
    :output-contract (natp (m-f5 x y))
    (if (< y -1)
        (+ (len x) (- -1 y))
        (+ (len x) (* 2 (+ y 1)))))

#|
==========================================
SECTION 2: Pre-defined Functions and Warm Up

We start with some familiar definitions just in case they
are useful. You will be asked to
define functions later on. Make sure to use defunc.
==========================================
(defunc listp (x)
  :input-contract t
  :output-contract (booleanp (listp x))
  (if (consp x)
      (listp (rest x))
    (equal x nil)))

(defunc app (a b) 
  :input-contract (and (listp a) (listp b))
  :output-contract (listp (app a b))
  (if (endp a)
      b
    (cons (first a) (app (rest a) b))))

(defunc rev (a) 
  :input-contract (listp a) 
  :output-contract (listp (rev a))
  (if (endp a)
      nil
    (app (rev (rest a)) (list (first a)))))

LHS > R
(defunc len (a) 
  :input-contract t 
  :output-contract (natp (len a))
  (if (atom a)
      0
    (+ 1 (len (rest a)))))
 
(defunc in (a X) 
  :input-contract (listp x)
  :output-contract (booleanp (in a X))
  (if (endp x)
      nil
    (if (equal a (first X))
      t
      (in a (rest X)))))

|#

:logic
;; Assume by "Def of lor" that each element is a rational
;; and a lor is (cons rational lor) | nil
(defdata lor (listof rational))

(defthm phi_applen (implies (and (listp l1) (listp l2))
                            (equal (len (app l1 l2))
                                   (+ (len l1)(len l2)))))
#|
PROVE
Let's do a warm-up proof (NOT GRADED)

Prove phi_applen: (implies (and (listp l1) (listp l2))
                           (equal (len (app l1 l2))
                                  (+ (len l1)(len l2))))
Make sure you clearly identify the induction scheme you are
using and what function the IS is from.
.........


|#

#|

|#
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; GIVEN
;; orderedp: lor -> boolean
;; (orderedp l) takes a list of rationals
;; and returns true if and only if the elements 
;; are in non-decreasing order (ie they are ordered)
(defunc orderedp (l)
  :input-contract (lorp l)
  :output-contract (booleanp (orderedp l))
  (or (endp l)
      (endp (rest l))
      (and (<= (first l) (second l))
           (orderedp (rest l)))))

#|
Sorting out Sorting 1
We will re-examine this for HW09 but I would like to consider merge sort and
selection sort.  Merge sort takes a list of rationals (at least for our 
implementation) and splits the list in half.  It keeps recursively doing so 
on both sublists until lists are length 1. The lists returned from the recursive
calls (which are assumed to be sorted) are then merged so the resultant list
is sorted.

Selection sort finds the minimum/smallest element in a list of rationals and 
adds it to the final list.  Then the next smallest element is removed and added 
to the returned list. When all element have been selected, the returned list is
sorted (we'll discuss what sorted means soon).

Before we reason about what these algorithms do, we need to make sure they are 
admissible....or theoretically so (msort is a pain).  For merge, msort, and ssort
PROVE each of these functions terminate.....however let's do some other proofs
first before proving msort and ssort terminate.....for no apparent reason.....
|#
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; GIVEN
;; merge: lor x lor -> lor
;; (merge x y) takes presumed ordered lists (of rationals) x and y and
;; merges them such that the resultant list is ordered.  This is done by
;; comparing the first of each list and taking the smaller value.
(defunc merge (x y)
  :input-contract (and (lorp x)(lorp y))
  :output-contract (lorp (merge x y))
  (cond ((endp x) y)
        ((endp y) x)
        ((<= (first x)(first y)) (cons (first x)(merge (rest x) y)))
        (t (cons (first y)(merge x (rest y))))))
#|
;; PROVE that merge terminates by writing a measure function and then proving
;; each proof obligation

(defunc m-merge (x y)
	:input-contract (and (lorp x) (lorp y))
	:output-contract (natp (m-merge x y))
	(+ (len x) (len y)))

Proof Obligations
1. ((first x) <= (first y)) /\ ~(endp x) /\ ~(endp y) => (m-merge x y) > (m-merge (rest x) y)
2. ~((first x) <= (first y)) /\ ~(endp x) /\ ~(endp y) => (m-merge x y) > (m-merge (rest x) y)

Case 1:
C1. ((first x) <= (first y))
C2. ~(endp x)
C3. ~(endp y)
--------------
C4. (len x) = (+ 1 (len a list of rationals
;; and returns true if and only if the elements 
;; are in non-decreasing order (ie they are ordered)
(defunc orderedp (l)
  :input-contract (lorp l)
  :output-contract (booleanp (orderedp l))
  (or (endp l)
      (endp (rest l))
      (and (<= (first l) (second l))
           (orderedp (rest l)))))

#|
Sorting out Sorting 1
We will re-examine this for HW09 but I would like to consider merge sort and
selection sort.  Merge sort takes a list of rationals (at least for our 
implementation) and splits the list in half.  It keeps recursively doing so 
on both sublists until lists are length 1. The lists returned from the recursive
calls (which are assumed to be sorted) are then merged so the resultant list
is sorted.

Selection sort finds the minimum/smallest element in a list of rationals and 
adds it to th(rest x))) {Def of len, C2}

LHS:(m-merge x y)
  = {Def of m-merge}
	(+ (len x) (len y))
  = {C4}
	(+ (+ 1 (len (rest x)) (len y))
  = {Associativity}
	(+ 1 (+ (len (rest x)) (len y)))
  = {RHS}
    (+ 1 RHS)
RHS:(m-merge (rest x) y)
  = {Def of m-merge}
	(+ (len (rest x)) (len y))

LHS > RHS, so Case 1 is t.

Case 2:
C1. ((first x) <= (first y))
C2. ~(endp x)
C3. ~(endp y)
--------------
C4. (len y) = (+ 1 (len (rest y))) {Def of len, C3}

LHS:(m-merge x y)
  = {Def of m-merge}
	(+ (len x) (len y))
  = {C4}
	(+ (len x) (+ 1 (len (rest y))))
  = {Associativity}
	(+ 1 (+ (len x) (len (rest y))))
  = {RHS}
    (+ 1 RHS)
RHS:(m-merge x (rest y))
  = {Def of m-merge}
	(+ (len x) (len (rest y)))

LHS > RHS, so Case 2 is t.
|#


(defunc first-half (l n)
  :input-contract (and (lorp l)(natp n))
  :output-contract (lorp (first-half l n))
  (cond ((endp l) nil)
        ((<= (len l) n) nil)
        (t (cons (first l)(first-half (rest l) (+ n 1))))))

(check= (first-half '(1 2 3) 0) '(1 2))
(check= (first-half '(1 2 3 4) 0) '(1 2))
(check= (first-half '(1 2 3 4 5) 0) '(1 2 3))
(check= (first-half '(1 2 3 4 5 6) 0) '(1 2 3))
(check= (first-half '(1) 0) '(1))
(check= (first-half nil 0) nil)

(defunc last-half (l n)
  :input-contract (and (lorp l)(natp n))
  :output-contract (lorp (last-half l n))
  (cond ((endp l) nil)
        ((<= (len l) n) l)
        (t (last-half (rest l) (+ n 1)))))

(check= (last-half '(1 2 3) 0) '(3))
(check= (last-half '(1 2 3 4) 0) '(3 4))
(check= (last-half '(1 2 3 4 5) 0) '(4 5))
(check= (last-half '(1 2 3 4 5 6) 0) '(4 5 6))
(check= (last-half '(1) 0) nil)
(check= (last-half nil 0) nil)

;; Here are some theorems ACL2s can do.
(defthm phi_lh_less (implies (and (lorp l)(natp n)(< n (len l)))
                             (< (len (last-half l n)) (len l))))

(defthm phi_fh_less (implies (and (lorp l)(consp l)(consp (rest l))(natp n))
                             (< (len (first-half l n)) (len l))))

#| PROVE the theorems above.  Make sure you identify your induction scheme each time
Notice, that if you want to USE these theorems later you can refer to them as phi_lh_less
and phi_fh_less.  Note you will get LOTS of context only proofs and a proof by cases
(at last the way I did it).  
Also note (without a proof):
(defthm phi_lh_eq (implies (and (lorp l)(natp n)(>= n (len l)))
                           (equal (len (last-half l n)) (len l))))

Induction Scheme (for both):
1. ~((lorp l) /\ (natp n)) => phi
2. (lorp l) /\ (natp n) /\ (endp l) => phi
3. (lorp l) /\ (natp n) /\ ~(endp l) /\ ((len l) <= n) => phi
4. (lorp l) /\ (natp n) /\ ~(endp l) /\ ~((len l) <= n) /\ phi | ((l (rest l)) (n (+ n 1))) => phi

Proof for phi_lh_less
Case 1. 
C1. ~((lorp l) /\ (natp n)) 
C2. (lorp l)
C3. (natp n)
C4. (< n (len l))
---------
C5. ~(lorp l) \/ ~(natp n) {C1, PL}
C6. f {C2, C3, C5}

f => phi_lh_less is t 

Case 2.
C1. (lorp l)
C2. (natp n)
C3. (endp l)
C4. (< n (len l))
-------------
C5. (len l) = 0 {C3, def of len}
C6. (>= n 0) {Def of nat, C2}
C7. (< n 0) {C4, C5}
C8. ~(>= n 0) {C7, Arithmetic}
C9. f {C8, C6}

f => phi_lh_less is t

Case 3.
C1. (lorp l)
C2. (natp n)
C3. ~(endp l)
C4. (<= (len l) n)
C5. (< n (len l))
--------------
C6. (>= n (len l)) {C4, Arithmetic}
C7. f {C5, C6}

f => phi_lh_less is t

Case 4.
C1. (lorp l)
C2. (natp n)
C3. ~(endp l)
C4. ~(>= n (len l))
C5. (< n (len l))
C6. (lorp (rest l)) /\ (natp (+ n 1)) => (< (len (last-half (rest l) (+ n 1))) (len (rest l))) {IS}
---------------------
C7. (lorp (rest l)) {C1, C3, Def of listp, def of endp}
C8. (natp (+ n 1)) {C2, def of natp}
C9. (< (len (last-half (rest l) (+ n 1))) (len (rest l))) {C9, C7, C6, MP}
C10. (len l) = (+ 1 (len (rest l))) {Def of len, C3}
C11. (- (len l) 1) < (len l) {Arithmetic}

LHS:(len (last-half l n))
  = {def of last-half}
    (len (last-half (rest l) (+ n 1)))
  < {C9}
    (len (rest l))
  < {C10, Arithmetic}
    (- (len l) 1)
  < (len l)

LHS < RHS, so Case 4 is t.

(defthm phi_fh_less (implies (and (lorp l)(consp l)(consp (rest l))(natp n))
                             (< (len (first-half l n)) (len l))))

Proof for phi_fh_less
Case 1. 
C1. ~((lorp l) /\ (natp n))
C2. (lorp l)
C3. (consp l)
C4. (consp (rest l))
C5. (natp n)
---------------
C6. ~(lorp l) \/ ~(natp n) {C1, PL}
C7. f {C2, C5, C6}

f => phi_fh_less is t.

Case 2. 
C1. (lorp l)
C2. (natp n)
C3. (endp l)
C4. (consp l)
C5. (consp (rest l))
------------
C6. f {C3, c4, def of endp}

f => phi_fh_less is t.

Case 3.
C1. (lorp l)
C2. (natp n)
C3. ~(endp l)
C4. (consp l)
C5. (<= (len l) n)
C6. (consp (rest l))
-----------
C7. (len l) = (+ 1 (len (rest l))) {C4, def of len}
C8. (len l) > 0 {L1, C1, C3, MP}

LHS:(len (first-half l n))
  = {def of first-half, C5}
    (len nil)
  = {def of len, def of endp}
    0
RHS:(len l)
  > 0
    {C8}
  > LHS

LHS < RHS, so Case 3 is t.

Case 4.
C1. (lorp l)
C2. (natp n)
C3. ~(endp l)
C4. ~(<= (len l) n)
C5. (consp l)
C6. (consp (rest l))
C7. (lorp (rest l)) /\ (natp (+ n 1)) => (< (len (first-half (rest l) (+ n 1))) (len (rest l))) {IS}
----------------------
C8. (lorp (rest l)) {C3, C1, C5}
C9. (natp (+ n 1)) {C2, Def of natp, Arithmetic}
C10. (< (len (first-half (rest l) (+ n 1))) (len (rest l))) {C8, C9, C7, MP}

LHS:(len (first-half l n))
  = {def of first-half}
    (len (cons (first l) (first-half (rest l) (+ n 1))))
  = {def of len, consp axiom, def of endp}
    (+ 1 (len (first-half (rest l) (+ n 1))))
  < {C10}
    (+ 1 (len (rest 1)))
  < {def of len}
    (len (cons (first l) (rest l)))
  < {first-rest axiom}
    (len l)
LHS < RHS, so Case 4 is t.

Lemma 1: (listp l) /\ ~(endp l) => (len l) > 0
C1. (listp l)
C2. ~(endp l)
-------------
C3. (consp l) {C2, Def of endp}
C4. (listp (rest l)) {C3, first-rest axiom, def of listp}
C5. (len l) = (+ 1 (len (rest l))) {C4, C1, C2, def of len}
C6. (natp (len (rest l))) {Contract of len}
C7. (len (rest l)) >= 0 {Def of nat}
C8. (len l) >= (+ 1 0) {C5, C7}
C9. (len l) >= 1 > 0 {C8, Arithmetic}
C10. (len l) > 0

The inverse is true as well
(listp l) /\ (len l) > 0 => ~(endp l)
C1. (listp l)
C2. (len l) > 0
---------------

Assume for contradiction.
C3. (endp l)
C4. (len l) = 0 {C3, def of len}

C4 and C2 cannot be true simultaneously, so C3 is f.


|#
:program
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; DEFINE
;; msort: lor -> lor
;;
;; (msort l) takes any list of rationals and sorts
;; it using merge sort. The sorted list is returned  
;; Merge sort works by splitting l in (approximately) half 
;; calling msort on each half and merging the sorted
;; lists. Thus you should be calling split-list-front,
;; split-list-back and merge. If the list is empty or length
;; 1 then stop.
(defunc msort (l)
  :input-contract (lorp l)
  :output-contract (lorp (msort l))
  (if (or (equal (len l) 1) (endp l))
      l
      (merge (msort (first-half l 0))
       	     (msort (last-half l 0)))))

;; ADD TESTS BELOW
(check= (msort '(1 2 3 4)) '(1 2 3 4))
(check= (msort '()) '())
(check= (msort '(3 4 1 2)) '(1 2 3 4))
(check= (msort '(1 2 1 2)) '(1 1 2 2))



; Prove that you are smarter than ACL2s: Prove
; that msort must terminate.....you will probably
; need to prove a couple theorems along the way (or use something given).
; If everyone is getting stuck, I can give a hint.
; MEASURE FUNCTION HERE
(defunc m-msort (l)
    :input-contract (lorp l)
    :output-contract (natp (m-msort l))
    (if (or (equal (len l) 1) (endp l))
        0
        (len l)))

 #|
 PROOF the measure function works here

Proof obligations
1. (listp l) /\ ~(equal (len l 1) /\ ~(endp l) => (m-msort l) > (m-msort (first-half l 0))
2. (listp l) /\ ~(equal (len l 1) /\ ~(endp l) => (m-msort l) > (m-msort (last-half l 0))


Obligation 1:
C1. (listp l)
C2. ~(equal (len l) 1)
C3. ~(endp l)
------------
C4. (consp l) {C3, def of endp}
C5. (len l) > 1 {C2, C2, C3, L2}
C6. (consp (rest l)) {C1, C5, L2}
C7. (natp 0) {Def of natp}

LHS:(m-msort l)
  = {def of m-msort}
    (len l)
RHS:(m-msort (first-half l 0))
  < {phi_fh_less | ((n 0)), C1, C4, C6, C7}
    (len l)
LHS > RHS, so Obligation 1 is proven.

Obligation 2:
C1. (listp l)
C2. ~(equal (len l) 1)
C3. ~(endp l)
---------------
C4. (natp 0} {Def of natp}
C5. (len l) > 1 {C2, C3, L1}
C6. (len 1) > 0 {C5, Arithmetic}

LHS:(m-msort l)
  = {def of m-msort}
    (len l)
RHS:(m-msort (first-half l 0))
  < {phi_lh_less | ((n 0)), C1, C4, C6}
    (len l)
LHS > RHS, so Obligation 2 is proven


Lemma 2: (listp l) /\ ~(equal (len l) 1) /\ ~(endp l) => (len l) > 1
C1. (listp l)
C2. ~((len l) = 1)
C3. ~(endp l)
-------------
C4. (len l) > 0 {L1, C1, C3, MP}
C5. (len l) = (+ 1 (len (rest l))) {Def of len, C1, C3}
C6. ~((len (rest l)) = 0) {C2, C5, Arithmetic}
C7. (natp (len (rest l))) {Contract of len}
C8. (len (rest l)) > 0 {C6, C7, Arithmetic}
C9. (len l) > (+ 1 0) {C5, C8}
C10. (len l) > 1 {C9, Arithmetic}

Lemma 2 Side Effect:
C11. ~(endp (rest l)) {C8, L1}
C12. (consp (rest l))


 |#

:logic

#|
min-l: LOR (non empty) -> Rational
Here I will define min-l because different implementations lead to different
proofs.  min-l takes a lorp and determines the smallest
number in the list.
NOTICE: I made this more efficient using let but this is equivalent to calling
the recursive call multiple times.  Also notice that if you have an I.S.
that min-l gives rise to, then EVERY recursive call (including in the if condition)
gives you an inductive assumption.....and notice I took the time to suggest
writing the I.S. for min-l
|#
(defunc min-l (l)
  :input-contract (and (lorp l)(consp l))
  :output-contract (rationalp (min-l l))
  (if (endp (rest l))                  
    (first l)
    (let ((minrest (min-l (rest l))))
      (if (>= minrest (first l)) 
        (first l)
        minrest))))

(check= (min-l '(1)) 1)
(check= (min-l '(1 5/2 17/2 13 -47)) -47)
(check= (min-l '(-48 5/2 17/2 13 -47)) -48)
(test? (implies (and (lorp l)(consp l))
                (in (min-l l) l)))

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

#|
Write selection sort (ssort) and then prove it terminates
Note this is NOT as trivial as it seems. You don't KNOW that
deleting an element from a list results in a smaller list.

Furthermore, is the following true? Adjust the conjecture so it is a theorem.
(listp l) /\ (in e l) => ((len l) > (len (del e l)))

Are we sure that (min-l l) is an element IN l?
(lorp l) /\ (consp l) => (in (min-l l) l)

|#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ssort: lor -> lor
;; DEFINE
;; (ssort l) or selection sort takes a lorp l as input,
;; and returns a permutation of l with elements in non-decreasing order.
;; ALGORITHM: if l is empty, return nil. If not,
;; find the smallest element in l and add to the sorted list.  
;; Keep SELECTING and removing the smallest element from the 
;; input list and cons it onto the sorted list (the output list 
;; from the recursive call).  Stop recursively calling ssort 
;; when l is empty. 
(defunc ssort (l)
	:input-contract (lorp l)
   	:output-contract (lorp (ssort l))
    (if (endp l)
        nil
        (cons (min-l l) (ssort (del (min-l l) l)))))

(defunc m-ssort (l)
    :input-contract (lorp l)
    :output-contract (natp (m-ssort l))
    (len l))

#|
Proof of termination

Induction Scheme:
1. ~(lorp l) => phi
2. (lorp l) /\ (endp l) => phi
3. (lorp l) /\ ~(endp l) /\ phi | ((l (del (min-l l) l))) => phi

Proof Obligations (for termination):
1. (lorp l) /\ ~(endp l) => (m-ssort l) > (m-ssort (del (min-l l) l))

Obligation 1:
C1. (lorp l)
C2. ~(endp l)
-------------
C3. (consp l) {C1, C2, def of endp}
C4. (in (min-l l) l) {L3, C3, MP}

LHS:(m-ssort l)
  = {Def of m-ssort}
    (len l)
RHS:(m-ssort (del (min-l l) l))
  = {Def of m-ssort}
    (len (del (min-l l) l))
  < {L4 | ((e (min-l l))), C4, C1, MP}
    (len l)
LHS > RHS, so the obligation is proven.

Lemma 3: (lorp l) /\ (consp l) => (in (min-l l) l)

Induction Scheme:
1. ~((lorp l) /\ (consp l)) => phi
2. (lorp l) /\ (consp l) /\ (endp (rest l)) => phi
3. (lorp l) /\ (consp l) /\ ~(endp (rest l)) /\ phi | ((l (rest l))) /\ (>= (min-l (rest l)) (first l)) => phi
4. (lorp l) /\ (consp l) /\ ~(endp (rest l)) /\ phi | ((l (rest l))) /\ ~(>= (min-l (rest l)) (first l)) => phi

Case 1.(lorp l) /\ (consp l) /\ ~(endp (rest l)) /\ phi | ((l (rest l))) /\ (>= (min-l (rest l)) (first l)) => phi
C1. (lorp l)
C2. (consp l)
C3. ~((lorp l) /\ (consp l)) 
---------------
C4. ~(lorp l) \/ ~(consp l) {C3, PL}
C5. f {C1, C2, C4}

f => phi is t.

Case 2.
C1. (lorp l)
C2. (consp l)
C3. (endp (rest l))
-------------
C4. ~(endp l) {C2, C1, def of endp}

RHS:(in (min-l l) l)
  = {def of min-l, C3}
    (in (first l) l)
  = {def of in, C4, first-rest axiom}
    (if (equal (first l) (first l)) 
        t
        (in (first l) (rest l)))
  = {equal axiom}
    t
LHS => RHS, so Case 2 is t.

Case 3.
C1. (lorp l)
C2. (consp l)
C3. ~(endp (rest l))
C4. (lorp (rest l)) /\ (consp (rest l)) => (in (min-l (rest l)) (rest l))
C5. (>= (min-l (rest l)) (first l))
-----------------------
C6. (lorp (rest l)) {C1, Def of lorp}
C7. (consp (rest l)) {C3, Def of endp}
C8. (in (min-l (rest l)) (rest l)) {C6, C7, C4, MP}
C9. ~(endp l) {C1, C2, def of endp}

RHS:(in (min-l l) l)
  = {def of min-l, C3, C5}
    (in (first l) l)
  = {def of in, C9, first-rest axiom}
    (if (equal (first l) (first l)) 
        t
        (in (first l) (rest l)))
  = {equal axiom}
    t
LHS => RHS, so Case 3 is t.

Case 4.
C1. (lorp l)
C2. (consp l)
C3. ~(endp (rest l))
C4. (lorp (rest l)) /\ (consp (rest l)) => (in (min-l (rest l)) (rest l))
C5. ~(>= (min-l (rest l)) (first l))
-----------------------
C6. (lorp (rest l)) {C1, Def of lorp}
C7. (consp (rest l)) {C3, Def of endp}
C8. (in (min-l (rest l)) (rest l)) {C6, C7, C4, MP}
C9. ~(endp l) {C1, C2, def of endp}

RHS:(in (min-l l) l)
  = {def of min-l, C3, C5}
    (in (min-l (rest l)) l)
  = {def of in, C9, first-rest axiom}
    (if (equal (min-l (rest l)) (first l)) 
        t
        (in (min-l (rest l)) (rest l)))
  = {C5}
    (in (min-l (rest l)) (rest l))
  = {C8}
    t
LHS => RHS, so Case 4 is t.

All Cases are t, so Lemma 3 (L3) is t.

(defunc in (a X) 
  :input-contract (listp x)
  :output-contract (booleanp (in a X))
  (if (endp x)
      nil
    (if (equal a (first X))
      t
      (in a (rest X)))))

Lemma 4: (listp l) /\ (in e l) => ((len l) > (len (del e l)))

Induction Scheme (for del):
1. ~(listp l) => phi
2. (listp l) /\ (endp l) => phi
3. (listp l) /\ ~(endp l) /\ ((first l) = e) => phi
4. (listp l) /\ ~(endp l) /\ ~((first l) = e) /\ phi | ((l (rest l))) => phi

Case 1.
C1. ~(listp l)
C2. (listp l)
C3. (in e l)
-------------
C4. f {C1, C2}

f => phi is t.

Case 2.
C1. (listp l)
C2. (endp l)
C3. (in e l)
------------- 
C4. f {Def of in, C2}

f => phi is t.

Case 3.
C1. (listp l)
C2. ~(endp l)
C3. (first l) = e
C4. (in e l)
-------------
C5. (len l) = (+ 1 (len (rest l))) {C2, Def of len}

LHS:(len l)
  = {C5}
    (+ 1 (len (rest l)))
  = {RHS}
    (+ 1 RHS)
RHS:(len (del e l))
  = {def of del, C2, C3}
    (len (rest l))

LHS > RHS, so Case 3 is t.

Case 4.
C1. (listp l)
C2. ~(endp l)
C3. ~((first l) = e)
C4. (in e l)
C5. (listp (rest l)) /\ (in e (rest l)) => (len (rest l)) > (len (del e (rest l)))
-------------
C6. (listp (rest l)) {Def of lisp, C2, Def of endp}
C7. (in e (rest l)) {Def of in, C2, C3}
C8. (len (rest l)) > (len (del e (rest l))) {C6, C7, C5, MP}
C9. (len l) = (+ 1 (len (rest l))) {C2, Def of len}

LHS:(len l)
  = {C9}
    (+ 1 (len (rest l)))
  > {C8}
    (+ 1 (del e (rest l)))
RHS:(len (del e l))
  = {Def of del, C2, C3}
    (len (cons (first l) (del e (rest l))))
  = {Def of len, first-rest axiom}
    (+ 1 (del e (rest l))

LHS > RHS, so Case 4 is t.

All the Cases are t, so Lemma 4 (L4) is t.
|#



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; SECTION XXX: Bubble Sort (HUGE NO CREDIT CHALLENGE)
;; Check out this known slow sorting function.
;; I have to throw things into program mode to let it
;; be admitted. Show that you are smarter than a computer
;; Prove that the function terminates. 
;; How do you know bsort is getting closer to a sorted list?
;; You may need to write another function
;; Even if you get a rough outline of the proof, that would
;; be a great exercise to demonstrate to yourself that you 
;; know what you are doing, but it's well outside of the scope
;; of this already long assignment.
;; DO NOT DO THIS PROBLEM UNTIL YOU FINISH THE REST OF THE ASSIGNMENT
;; It really is worth no points.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

:program
;#.....#

(defunc bubble (l)
  :input-contract (lorp l)
  :output-contract (lorp (bubble l))
  (cond ((endp l) l)
        ((endp (rest l)) l)
        ((> (first l)(second l))(cons (second l)(bubble (cons (first l) (rest (rest l))))))
        (t (cons (first l)(bubble (rest l))))))

(defunc bsort (l)
  :input-contract (lorp l)
  :output-contract (lorp (bsort l))
  (if (orderedp l)
    l
    (bsort (bubble l))))

;#.....#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Feedback (5 points)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
#|

Every couple of weeks we will ask a couple questions to monitor how the course
is progressing.  This won't be as long as the HW06 survey. 

Please fill out the following form.

https://goo.gl/forms/KBcsUC4lQg91uN0u2

As before, feedback is anonymous and each team member should fill out
the form (only once per person).

After you fill out the form, write your name below in this file, not
on the questionnaire. We have no way of checking if you submitted the
file, but by writing your name below you are claiming that you did,
and we'll take your word for it.  

5 points will be awarded to each team member for filling out the 
questionnaire.  More importantly, if a course topic is confusing you, I can
address the issue without you having to worry that only you don't get the topic
(Note: it's almost never the case that only 1 student is confused but I know
it can still be scary)

The following team members filled out the feedback survey provided by 
the link above:
---------------------------------------------
Kevin Zhang
<firstname> <LastName>

|#

```


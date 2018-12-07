```lisp
; **************** BEGIN INITIALIZATION FOR ACL2s B MODE ****************** ;
; (Nothing to see here!  Your actual file is after this initialization code);

#|
Pete Manolios
Fri Jan 27 09:39:00 EST 2012
----------------------------

Made changes for spring 2012.


Pete Manolios
Thu Jan 27 18:53:33 EST 2011
----------------------------

The Beginner level is the next level after Bare Bones level.

|#

; Put CCG book first in order, since it seems this results in faster loading of this mode.
#+acl2s-startup (er-progn (assign fmt-error-msg "Problem loading the CCG book.~%") (value :invisible))
(include-book "acl2s/ccg/ccg" :uncertified-okp nil :dir :system :ttags ((:ccg)) :load-compiled-file nil);v4.0 change

;Common base theory for all modes.
#+acl2s-startup (er-progn (assign fmt-error-msg "Problem loading ACL2s base theory book.~%") (value :invisible))
(include-book "acl2s/base-theory" :dir :system :ttags :all)

#+acl2s-startup (er-progn (assign fmt-error-msg "Problem loading ACL2s customizations book.~%") (value :invisible))
(include-book "custom" :dir :acl2s-modes :uncertified-okp nil :ttags :all)

;Settings common to all ACL2s modes
(acl2s-common-settings)

#+acl2s-startup (er-progn (assign fmt-error-msg "Problem loading trace-star and evalable-ld-printing books.~%Please choose \"Recertify ACL2s system books\" under the ACL2s menu and retry after successful recertification.") (value :invisible))
(include-book "trace-star" :uncertified-okp nil :dir :acl2s-modes :ttags ((:acl2s-interaction)) :load-compiled-file nil)
(include-book "hacking/evalable-ld-printing" :uncertified-okp nil :dir :system :ttags ((:evalable-ld-printing)) :load-compiled-file nil)

;theory for beginner mode
#+acl2s-startup (er-progn (assign fmt-error-msg "Problem loading ACL2s beginner theory book.~%Please choose \"Recertify ACL2s system books\" under the ACL2s menu and retry after successful recertification.") (value :invisible))
(include-book "beginner-theory" :dir :acl2s-modes :ttags :all)


#+acl2s-startup (er-progn (assign fmt-error-msg "Problem setting up ACL2s Beginner mode.") (value :invisible))
;Settings specific to ACL2s Beginner mode.
(acl2s-beginner-settings)

; why why why why 
(acl2::xdoc acl2s::defunc) ; almost 3 seconds

(cw "~@0Beginner mode loaded.~%~@1"
    #+acl2s-startup "${NoMoReSnIp}$~%" #-acl2s-startup ""
    #+acl2s-startup "${SnIpMeHeRe}$~%" #-acl2s-startup "")


(acl2::in-package "ACL2S B")

; ***************** END INITIALIZATION FOR ACL2s B MODE ******************* ;
;$ACL2s-SMode$;Beginner
#|
CS 2800 Homework 9 - Fall 2018


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

 * Submit the homework file (this file) on Blackboard. Do not rename 
   this file. There will be a 10 point penalty for this.

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
 Inductive Proofs:
 - For all proofs below, solutions must be in the format described in class and
   in the notes. This includes:
     * Explicitly identifying an induction scheme and the function that gives
       rise to it.
     * Labeling general context (C1, C2....) and derived context.
     * Providing justifications for each piece of derived context.
     * Explicitly identifying axioms and theorems used
     * The if axioms and theorem substitutions are not required. You can use
       any other shortcuts previously identified.
     * PL can be given as justification for any propositional logic rules with the
      exceptions of Modus Ponens (MP) and Modus Tollens (MT)
     * All arithmetic rules can be justified by writing "Arithmetic".
     
Previous homeworks (such as HW05) identify these requirements in more detail.

|#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
#|
For this homework you may want to use ACL2s to help you.

Technical instructions:

- open this file in ACL2s as hw09.lisp

- make sure you are in BEGINNER mode. This is essential! Note that you can
  only change the mode when the session is not running, so set the correct
  mode before starting the session.

- insert your solutions into this file where indicated (usually as "...")

- only add to the file. Do not remove or comment out anything pre-existing
  unless asked to.

- make sure the entire file is accepted by ACL2s. In particular, there must
  be no "..." left in the code. If you don't finish all problems, comment
  the unfinished ones out. Comments should also be used for any English
  text that you may add. This file already contains many comments, so you
  can see what the syntax is.

- when done, save your file and submit it as hw09.lisp

- avoid submitting the session file (which shows your interaction with the
  theorem prover). This is not part of your solution. Only submit the lisp
  file!

|#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; GIVEN FUNCTIONS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|
We start with some familiar definitions just in case they
are useful. 
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

(defunc len (a) 
  :input-contract (listp a)
  :output-contract (natp (len a))
  (if (endp a)
      0
    (+ 1 (len (rest a)))))
 
(defunc in (a X) 
  :input-contract (listp x)
  :output-contract (booleanp (in a X))
  (cond ((endp x) nil)
        ((equal a (first X)) (in a (rest X)))
        (t nil)))

|#

;; Assume by "Def of lor" that each element is a rational
;; and a lor is (cons rational lor) | nil
(defdata lor (listof rational))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; GIVEN
;; orderedp: lor -> boolean
;; (orderedp l) takes a list of rationals
;; and returns true if and only if the elements 
;; are in non-decreasing order (ie they are ordered)
(defunc orderedp (l)
  :input-contract (lorp l)
  :output-contract (booleanp (orderedp l))
  (cond ((or (endp l)(endp (rest l))) t)
        ((<= (first l) (second l)) (orderedp (rest l)))
        (t nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; GIVEN
;; del: Any x List -> List
;; (del e l) takes an element e and a list l
;; and removes the FIRST occurance of e from l
;; If e is not in l then l is returned.
(defunc del (e l)
  :input-contract (listp l)
  :output-contract (listp (del e l))
  (cond ((endp l) l)
        ((equal e (first l)) (rest l))
        (t (cons (first l) (del e (rest l))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; GIVEN
;; perm: List x List -> List
;; (perm l1 l2) takes two lists (l1 and l2) and
;; returns true if and only if l1 and l2 have
;; the same elements (and the same number of each)
;; Essentially, is l2 a reordering of l1.
(defunc perm (l1 l2)
  :input-contract (and (listp l1)(listp l2))
  :output-contract (booleanp (perm l1 l2))
  (cond ((endp l1) (endp l2))
        ((in (first l1) l2) (perm (rest l1)(del (first l1) l2)))
        (t nil)))

;;==========================================
; Section 1: Permutation "Fun"
;;==========================================
#|
As a general rule, proofs involving permutations seem simple
but are a royal pain to prove (if you don't believe me, ask someone
who took 2800 this past Spring. They did a lot of permutation proofs)
Let's start with something "simple".  Prove the following

phi_perm_appcons: (listp x)/\(listp y)=> (perm (app x (cons e y))(cons e (app x y)))

** You may assume (perm l l) is always true.
   phi_perm_refl: (listp l) => (perm l l)
** You may also assume (perm x y)/\(perm y z) => (perm x z) is a theorem
   phi_perm_trans: (listp x)/\ (listp y)/\(listp z) /\ (perm x y)/\ (perm y z)=> (perm x z)
   It can also be written as:
   (listp x) /\ (listp y) /\ (listp z) /\ (perm y z) => ((perm x y)=(perm x z))

Induction Scheme:
1. ~((listp x) /\ (listp y)) => phi
2. (listp x) /\ (listp y) /\ (endp x) => phi
3. (listp x) /\ (listp y) /\ ~(endp x) /\ phi | (x (rest x)) => phi

Obligation 1:
C1. ~((listp x) /\ (listp y))
C2. (listp x) {Contract of app}
C3. (listp y) {Contract of app}
--------------
C4. f {C1, C2, C3}

f => phi_perm_appcons is t.

Obligation 2:
C1. (listp x)
C2. (listp y)
C3. (endp x)
----------------

    (perm (app x (cons e y)) (cons e (app x y)))
  = {def of app, C3}
	(perm (cons e y) (cons e y))
  = {phi_perm_refl | (l (cons e y))}
    t

t => phi_perm_appcons is t.

Oblgation 3:
C1. (listp x)
C2. (listp y)
C3. ~(endp x)
C5. (listp (rest x)) /\ (listp y) => (perm (app (rest x) (cons e y)) (cons e (app (rest x) y))
-------------
C6. (consp x) {C3, def of endp}
C7. (listp (rest x)) {C1, C6, def of listp}
C8. (perm (app (rest x) (cons e y)) (cons e (app (rest x) y))) {C7, C2, C5, MP}

    (perm (app x (cons e y)) (cons e (app x y)))
  = {def of app, C3}
	(perm (cons (first x) (app (rest x) (cons e y)))
          (cons e (app x y)))
  = {def of perm, def of endp, consp axiom, first-rest axiom}
    (if (in (first x) (cons e (app x y)))
        (perm (app (rest x) (cons e y))
              (del (first x) (cons e (app x y)))
        nil)
  = {L1, C6, C2}
    (perm (app (rest x) (cons e y))
          (del (first x) (cons e (app x y))))
  = {L2, C6, C2}
    (perm (app (rest x) (cons e y))
          (cons e (app (rest x) y)))
  = {C8}
    t

t => phi_perm_appcons is t.


Lemma 1:(consp x) /\ (listp y) => (in (first x) (cons e (app x y)))

Case 1:
C1. (consp x)
C2. (listp y)
C3. (first x) = e
---------------

   (in (first x) (cons e (app x y)))
 = {def of in, C3}
   t

Case 1 is t.

Case 2:
C1. (consp x)
C2. (listp y)
C3. ~(first x) = e
----------------

   (in (first x) (cons e (app x y)))
 = {def of in, C3, first-rest axiom}
   (in (first x) (app x y))
 = {Def of app, C1, def of endp}
   (in (first x) (cons (first x) (app (rest x) y)))
 = {Def of in, def of endp, consp axiom, first-rest axiom}
   (if (equal (first x) (first x))
       t
       (in (first x) (app (rest x) y)))
 = {Equal axiom}
   t

Case 2 is t.

Lemma 2:(consp x) /\ (listp y) => (del (first x) (cons e (app x y))) = (cons e (app (rest x) y))

Case 1.
C1. (consp x)
C2. (listp y)
C3. (first x) = e
----------------

LHS:(del (first x) (cons e (app x y)))
  = {def of del, def of endp, consp axiom, C3}
    (app x y)
  = {def of app, C1}
    (cons (first x) (app (rest x) y))
  = {C3}
    (cons e (app (rest x) y))
RHS:(cons e (app (rest x) y))

LHS = RHS, so Case 1 is t.

Case 2.
C1. (consp x)
C2. (listp y)
C3. ~(first x) = e
---------------

LHS:(del (first x) (cons e (app x y)))
  = {def of del, def of endp, consp axiom, C3}
    (cons e (del (first x) (app x y)))
  = {def of app, C1, def of endp}
    (cons e (del (first x) (cons (first x) (app (rest x) y))))
  = {def of del, def of endp, consp axiom, first-rest axiom}
    (cons e (app (rest x) y))
RHS:(cons e (app (rest x) y))

LHS = RHS, so Case 2 is t.

All Obligations are t, so phi_perm_appcons is t.

Now prove the following:
phi_perm_app: (listp x)/\(listp y) => (perm (app x y)(app y x))

We can assume (from class)
phi_app_nil: (listp x) => (app x nil) = x

Induction Scheme:
1. ~((listp x) /\ (listp y)) => phi
2. (listp x) /\ (listp y) /\ (endp x) /\ (endp y) => phi
3. (listp x) /\ (listp y) /\ ~(endp x) /\ (endp y) => phi
4. (listp x) /\ (listp y) /\ (endp x) /\ ~(endp y) => phi
5. (listp x) /\ (listp y) /\ ~(endp x) /\ ~(endp y) /\ phi | ((x (rest x)) (y (rest y))) => phi

Obligation 1.
C1. ~((listp x) /\ (listp y))
C2. (listp x) {Contract of app}
C3. (listp y) {Contract of app}
-----------------
C4. f {C1, C2, C3}

f => phi_perm_app is t.

Obligation 2.
C1. (listp x)
C2. (listp y)
C3. (endp x)
C4. (endp y)
------------------

    (perm (app x y) (app y x))
  = {def of app, C3, C4}
    (perm nil nil)
  = {phi_perm_refl | ((l nil))}
    t

t => phi_perm_app is t.

Obligation 3.
C1. (listp x)
C2. (listp y)
C3. ~(endp x)
C4. (endp y)
---------------------
C5. y = nil {C4, def of endp}

    (perm (app x y) (app y x))
  = {C5}
    (perm (app x nil) (app nil x))
  = {def of app, def of endp}
    (perm (app x nil) x)
  = {phi_app_nil, C1}
    (perm x x)
  = {phi_perm_refl, C1}
    t

t => phi_perm_app is t.

Obligation 4.
C1. (listp x)
C2. (listp y)
C3. (endp x)
C4. ~(endp y)
---------------------
C5. x = nil {C3, def of endp}

    (perm (app x y) (app y x))
  = {C5}
    (perm (app nil y) (app y nil))
  = {def of app, def of endp}
    (perm y (app y nil))
  = {phi_app_nil, C2}
    (perm y y)
  = {phi_perm_refl, C2}
    t

t => phi_perm_app is t.

Obligation 5.
C1. (listp x)
C2. (listp y)
C3. ~(endp x)
C4. ~(endp y)
C5. (listp (rest x)) /\ (listp y) => (perm (app (rest x) y) (app y (rest x)))
--------------------
C6. (consp x) {C3, def of endp}
C7. (listp (rest x)) {C1, C6, def of listp}
C8. (perm (app (rest x) y) (app y (rest x))) {C7, C2, C5, MP}

    (perm (app x y) (app y x))
  = {def of app, C3}
    (perm (cons (first x) (app (rest x) y))
          (app y x))
  = {def of perm, def of endp, consp axiom, first-rest axiom}
    (if (in (first x) (app y x))
        (perm (app (rest x) y) 
              (del (first x) (app y x)))
        nil)
  = {L3}
    (perm (app (rest x) y)
          (del (first x) (app y x)))
  = {L4, phi_perm_trans | ((X (app (rest x) y))) (Y (del (first x) (app y x))) (Z (app y (rest x))))}
    (perm (app (rest x) y)
          (app y (rest x)))
  = {C8}
    t

t => phi_perm_app is t.
  

All obligations have been proven, so phi_perm_app is t.


Lemma 3: (consp x) /\ (listp y) => (in (first x) (app y x))

Case 1:
C1. (consp x)
C2. (listp y)
C3. (endp y)
---------------

    (in (first x) (app y x))
  = {Def of app, C3}
    (in (first x) x)
  = {Def of in, C1, first-rest axiom}
    (if (equal (first x) (first x))
        t
        (in (first x) (rest x)))
  = {equal axiom, if axiom}
    t

Case 1 is t.

Case 2:
C1. (consp x)
C2. (listp y)
C3. ~(endp y)
C4. (first y) = (first x)
------------------------

    (in (first x) (app y x))
  = {def of app, C3}
    (in (first x) (cons (first y) (app (rest y) x)))
  = {def of in, def of endp, first-rest axiom}
    (if (equal (first x) (first y)) 
        t
       (in (first x) (app (rest y) x)))
  = {C4}
    t

Case 2 is t.

Case 3:
C1. (consp x)
C2. (listp y)
C3. ~(endp y)
C4. ~((first y) = (first x))
C5. (consp x) /\ (listp (rest y)) => (in (first x) (app (rest y) x))
-----------------------
C6. (listp (rest y)) {C3, def of endp, C2, def of listp}
C7. (in (first x) (app (rest y))) {C6, C5, MP}

    (in (first x) (app y x))
  = {def of app, C3}
    (in (first x) (cons (first y) (app (rest y) x)))
  = {def of in, def of endp, first-rest axiom}
    (if (equal (first x) (first y)) 
        t
       (in (first x) (app (rest y) x)))
  = {C4}
    (in (first x) (app (rest y) x))
  = {C7}
    t

Case 3 is t.

Lemma 3 is proven.

Lemma 4: (consp x) /\ (listp y) /\ (listp x) => (perm (del (first x) (app y x)) (app y (rest x)))

Case 1: 
C1. (consp x)
C2. (listp y)
C3. (endp y)
C4. (listp x)
---------------------------------

LHS:(del (first x) (app y x)) 
  = {Def of app, C3}
	(del (first x) x)
  = {Def of del, C1}
	(if (equal (first x) (first x)) 
        (rest x)
	    (cons (first l)(del e (rest l)))
  = {if axiom, equal axiom}
	(rest x)
RHS:(app y (rest x))
  = {Def of app, C3}
	(rest x)
-----------------
    (perm LHS RHS)
  = {phi_perm_refl, LHS, RHS}
    t

Case 1 is t.


Case 2.
C1. (consp x)
C2. (listp y)
C3. ~(endp y)
C4. (first x) = (first y)
C5. (listp x)
-----------------
C6. (listp (rest y)) {C2, C3, def of listp}
C7. (listp (rest x)) {C1, C5, def of listp}

    (perm (del (first x) (app y x)) 
          (app y (rest x)))
  = {Def of app, C3}
	(perm (del (first x) (cons (first y)(app (rest y) x))))
          (app y (rest x)))
  = {Def of del, Def of endp, consp axiom, first-rest axiom}
	(perm (if (equal (first x) (first y))
		      (app (rest y) x))
			  (cons (first y) (del (first x) (app (rest y) x))))
          (app y (rest x)))
  = {C4, if axiom}
    (perm (app (rest y) x)
		  (app y (rest x)))
  = {first-rest axiom}
    (perm (app (rest y) (cons (first x) (rest x)))
          (app (cons (first y) (rest y)) (rest x)))
  = {def of app, def of endp, consp axiom}
    (perm (app (rest y) (cons (first x) (rest x)))
          (cons (first y) (app (rest y) (rest x))))
  = {C4}
    (perm (app (rest y) (cons (first x) (rest x)))
          (cons (first x) (app (rest y) (rest x))))
  = {phi_perm_appcons | ((e (first x) (x (rest x)) (y (rest y)))), C6, C7}
    t

Case 2 is t.

Case 3.
C1. (consp x)
C2. (listp y)
C3. ~(endp y)
C4. ~((first x) = (first y))
C5. (consp x) /\ (listp (rest y)) => (perm (del (first x) (app (rest y) x))
                                           (app (rest y) (rest x)))
C6. (listp x)
----------------------
C7. (listp (rest y)) {C3, C2, def of listp}
C8. (perm (del (first x) (app (rest y) x))
          (app (rest y) (rest x))) {C5, C7, MP}


    (perm (del (first x) (app y x))
          (app y (rest x)))
  = {def of app, C3}
    (perm (del (first x) (cons (first y) (app (rest y) x))))
          (app y (rest x))
  = {def of del, def of endp, consp axiom, first-rest axiom}
 	(perm (if (equal (first x) (first y))
		      (app (rest y) x))
			  (cons (first y) (del (first x) (app (rest y) x))))
          (app y (rest x)))
  = {C4}
    (perm (cons (first y) (del (first x) (app (rest y) x))))
          (app y (rest x)))
  = {def of perm, def of endp, consp axiom, first-rest axiom}
    (if (in (first y) (app y (rest x))
        (perm (del (first x) (app (rest y) x))
              (del (first y) (app y (rest x)))
        nil))
  = {L1 | ((x y) (y (rest x))}
    (perm (del (first x) (app (rest y) x))
          (del (first y) (app y (rest x))))
  = {def of del, C3, first-rest axiom}
    (perm (del (first x) (app (rest y) x))
          (if (equal (first y) (first (app y (rest x)))) 
              (rest (app y (rest x)))
              (cons (first (app y (rest x)) (del (first y) (rest (app y (rest x)))))))
  = {def of app, first-rest axiom}
    (perm (del (first x) (app (rest y) x))
          (if (equal (first y) (first y))
              (app (rest y) x)
              (cons (first y) (del (first y) (app (rest y) x)))))
  = {equal axiom}
    (perm (del (first x) (app (rest y) x))
          (app (rest y) x))
  = {C8}
    t

Case 3 is t.

Lemma 4 is proven.

|#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; GIVEN
;; sortedp: LOR x LOR -> Boolean
;; (sortedp origL sortL) takes the original list
;; and the theoretically sorted list (sortL)
;; and determines if sortL is a sorteding
;; of the original list
(defunc sortedp (origL sortL )
  :input-contract (and (lorp origL)(lorp sortL))
  :output-contract (booleanp (sortedp origL sortL))
  (and (perm origL sortL)(orderedp sortL)))

#|
Section 2:
Sorting out Sorting 2
Back to merge sort and selection sort.  Let's prove some characteristics about them.....
not all because perm ends up being a giant pain yet again.  
REMEMBER: you can use any theorem that you have already proven.  This can be from this homework
a previous homework or an in-class exercise.  Once you have proven something then it is true
for all time....so you may want to look at your theorems from HW08....or at least the HW08 solutions.
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
Prove the following conjecture using the Induction scheme from merge:
phi_merge_ordered: (implies (and (lorp l1)(lorp l2)(orderedp l1)(orderedp l2))
                            (orderedp (merge l1 l2)))

To save you time, you just need to prove a single endp case
and one recursive case.  Your recursive case might require additional cases.

Induction Scheme:
1. ~((lorp x) /\ (lorp y)) /\ (lorp x) /\ (lorp y) /\ (orderedp x) /\ (orderedp y) => phi
2. (lorp x) /\ (lorp y) /\ (endp x) /\ (orderedp x) /\ (orderedp y) => phi
3. (lorp x) /\ (lorp y) /\ ~(endp x) /\ (endp y) /\ (orderedp x) /\ (orderedp y) => phi
4. (lorp x) /\ (lorp y) /\ ~(endp x) /\ ~(endp y) /\ (<= (first x) (first y)) /\ phi | ((x (rest x))) /\ (orderedp x) /\ (orderedp y)=> phi
5. (lorp x) /\ (lorp y) /\ ~(endp x) /\ ~(endp y) /\ ~(<= (first x) (first y)) /\ phi | ((y (rest y))) /\ (orderedp x) /\ (orderedp y) => phi

We will show proof for Obligations 1, 2, and 4. 

Obligation 1:
C1. ~((lorp x) /\ (lorp y)) 
C2. (lorp x) 
C3. (lorp y) 
C4. (orderedp x)
C5. (orderedp y)
----------------
C4. f {C1, C2, C3}

f => phi_merge_ordered is t, so Obligation 1 is proven.

Obligation 2:
C1. (lorp x) 
C2. (lorp y)
C3. (endp x)
C4. (orderedp x)
C5. (orderedp y)
--------------

    (orderedp (merge x y))
  = {def of merge, C3}
    (orderedp y)
  = {C5}
    t

Obligation 2 is proven.

Obligation 4:

Case 1:
C1. (lorp x)
C2. (lorp y)
C3. ~(endp x)
C4. ~(endp y)
C5. (<= (first x) (first y)) 
C6. (lorp (rest x)) /\ (lorp y) /\ (orderedp (rest x)) /\ (orderedp y) => (orderedp (merge (rest x) y))
C7. (orderedp x)
C8. (orderedp y)
C9. (endp (rest x)) 
--------------------
C10. (second (cons (first x) (merge (rest x) y))) = (first (merge (rest x) y)) {Def of second, first-rest axiom}
C11. (lorp (rest x)) {C1, C3, Def of lorp, def of endp, consp axiom}
C12. (orderedp (rest x)) {C9, def of orderedp}
C13. (orderedp (merge (rest x) y)) {C6, C11, C2, C12, C8, MP}

    (orderedp (merge x y))
  = {Def of merge, C3, C4, C5}
    (orderedp (cons (first x) (merge (rest x) y)))
  = {Def of orderedp, first-rest axiom}
    (if (or (endp (cons (first x) (merge (rest x) y)))
            (endp (merge (rest x) y)))
        t
        (if (<= (first (cons (first x) (merge (rest x) y)))
                (second (cons (first x) (merge (rest x) y)))
            (orderedp (merge (rest x) y))
            nil)))
  = {def of endp, consp axiom, PL, first-rest axiom, C9}
    (if (endp (merge (rest x) y))
        t
        (if (<= (first x) (first (merge (rest x) y)))
            (orderedp (merge (rest x) y))
            nil))
  = {L5 | (x (rest x)), C2, C4, C11, C9}
	(if (<= (first x) (first (merge (rest x) y)))
        (orderedp (merge (rest x) y))
        nil)
  = {C9, def of merge}
    (if (<= (first x) (first y))
        (orderedp (merge (rest x) y))
        nil)
  = {C5]
    (orderedp (merge (rest x) y))
  = {C13}
    t

Case 2:
C1. (lorp x)
C2. (lorp y)
C3. ~(endp x)
C4. ~(endp y)
C5. (<= (first x) (first y)) 
C6. (lorp (rest x)) /\ (lorp y) /\ (orderedp (rest x)) /\ (orderedp y) => (orderedp (merge (rest x) y))
C7. (orderedp x)
C8. (orderedp y)
C9. ~(endp (rest x)) 
--------------------
C10. (second (cons (first x) (merge (rest x) y))) = (first (merge (rest x) y)) {Def of second, first-rest axiom}
C11. (lorp (rest x)) {C1, C3, Def of lorp, def of endp, consp axiom}
C12. (and (<= (first x) (second x)) (orderedp (rest x))) {def of orderedp, C7, C3, C9}
C13. (orderedp (rest x)) {C12, PL}
C14. (orderedp (merge (rest x) y)) {C6, C11, C2, C13, C8, MP}
C15. (<= (first x) (first (rest x))) {C12, PL, def of second, first-rest axiom}

    (orderedp (merge x y))
  = {Def of merge, C3, C4, C5}
    (orderedp (cons (first x) (merge (rest x) y)))
  = {Def of orderedp, first-rest axiom}
    (if (or (endp (cons (first x) (merge (rest x) y)))
            (endp (merge (rest x) y)))
        t
        (if (<= (first (cons (first x) (merge (rest x) y)))
                (second (cons (first x) (merge (rest x) y)))
            (orderedp (merge (rest x) y))
            nil)))
  = {def of endp, consp axiom, PL, first-rest axiom, C9}
    (if (endp (merge (rest x) y))
        t
        (if (<= (first x) (first (merge (rest x) y)))
            (orderedp (merge (rest x) y))
            nil))
  = {L5 | (x (rest x)), C2, C4, C9, C11}
	(if (<= (first x) (first (merge (rest x) y)))
        (orderedp (merge (rest x) y))
        nil)
Case A. (<= (first (rest x)) (first y))
  = {def of merge, C2, C4, C11, C9, first-rest axiom}
    (if (<= (first x) (first (rest x))
        (orderedp (merge (rest x) y))
        nil)
  = {C15, if axiom}
    (orderedp (merge (rest x) y))
  = {C14}
    t
Case B. ~(<= (first (rest x)) (first y))
  = {def of merge, C2, C4, C9, C11, first-rest axiom}
    (if (<= (first x) (first y))
        (orderedp (merge (rest x) y))
        nil)
  = {C5}
    (orderedp (merge (rest x) y))
  = {C14}
    t

Obligation 4 is proven.
  
Lemma 5: (or ~(endp x) ~(endp y)) /\ (lorp x) /\ (lorp y) => ~(endp (merge x y))

Case 1:
C1. (endp x)
C2. ~(endp y)
C3. (lorp x)
C4. (lorp y)
-------------

   ~(endp (merge x y))
 = {def of merge, C1}
   ~(endp y)
 = {C2}
   t

Case 1 is t.

Case 2:
C1. ~(endp x)
C2. (endp y)
C3. (lorp x)
C4. (lorp y)
-------------

    ~(endp (merge x y))
  = {def of merge, C2}
    ~(endp x)
  = {C1}
    t

Case 2 is t.

Case 3:
C1. ~(endp x)
C2. ~(endp y)
C3. (lorp x)
C4. (lorp y)
C5. (<= (first x) (first y))
--------------

    ~(endp (merge x y))
  = {def of merge, C1, C2, C5}
    ~(endp (cons (first x) (merge (rest x) y)))
  = {def of endp, consp axiom}
    t

Case 3 is t.

Case 4:
C1. ~(endp x)
C2. ~(endp y)
C3. (lorp x)
C4. (lorp y)
C5. ~(<= (first x) (first y))
--------------

    ~(endp (merge x y))
  = {def of merge, C1, C2, C5}
    ~(endp (cons (first y) (merge x (rest y))))
  = {def of endp, consp axiom}
    t

Case 4 is t.

Lemma 5 is proven.



|#

#|

Prove the following using the Induction scheme that merge
gives rise to (notice WHY we want to use merge).

phi_merge_perm: (implies (and (lorp l1)(lorp l2)(orderedp l1)(orderedp l2))
                              (perm (app l1 l2) (merge l1 l2))))
                                
To save you time, you just need to prove a single endp case
and one recursive case.

Induction Scheme:
1. ~((lorp x) /\ (lorp y)) /\ (lorp x) /\ (lorp y) /\ (orderedp x) /\ (orderedp y) => phi
2. (lorp x) /\ (lorp y) /\ (endp x) /\ (orderedp x) /\ (orderedp y) => phi
3. (lorp x) /\ (lorp y) /\ ~(endp x) /\ (endp y) /\ (orderedp x) /\ (orderedp y) => phi
4. (lorp x) /\ (lorp y) /\ ~(endp x) /\ ~(endp y) /\ (<= (first x) (first y)) /\ phi | ((x (rest x))) /\ (orderedp x) /\ (orderedp y)=> phi
5. (lorp x) /\ (lorp y) /\ ~(endp x) /\ ~(endp y) /\ ~(<= (first x) (first y)) /\ phi | ((y (rest y))) /\ (orderedp x) /\ (orderedp y) => phi

We will show proof for Obligations 1, 2, and 4.

Obligation 1:
C1. ~((lorp x) /\ (lorp y)) 
C2. (lorp x) 
C3. (lorp y) 
C4. (orderedp x)
C5. (orderedp y)
----------------
C4. f {C1, C2, C3}

f => phi_merge_perm is t, so Obligation 1 is proven.

Obligation 2:
C1. (lorp x) 
C2. (lorp y)
C3. (endp x)
C4. (orderedp x)
C5. (orderedp y)
------------------

    (perm (app x y) (merge x y))
  = {def of app, def of merge, C3}
    (perm y y)
  = {phi_perm_refl}
    t

Obligation 2 is proven.

Recall:
phi_perm_appcons: (listp x)/\(listp y)=> (perm (app x (cons e y))(cons e (app x y)))
phi_perm_app: (listp x)/\(listp y) => (perm (app x y)(app y x))

Obligation 4:
C1. (lorp x)
C2. (lorp y)
C3. ~(endp x)
C4. ~(endp y)
C5. (<= (first x) (first y)) 
C6. (lorp (rest x)) /\ (lorp y) /\ (orderedp (rest x)) /\ (orderedp y) => (perm (app (rest x) y) (merge (rest x) y))
C7. (orderedp x)
C8. (orderedp y)
------------------
C9. (lorp (rest x)) {C1, C3, def of lorp, def of endp, consp axiom}
C10. (orderedp (rest x)) {L6, C7}
C11. (perm (app (rest x) y) (merge (rest x) y)) {C6, C9, C10, MP}


	(perm (app x y) (merge x y))
  = {def of merge, C3, C4, C5}
    (perm (app x y) 
          (cons (first x) (merge (rest x) y)))
  = {def of app, C3}
    (perm (cons (first x) (app (rest x) y))
          (cons (first x) (merge (rest x) y)))
  = {L7 | ((x (app (rest x) y)) (y (merge (rest x) y)))}
    (perm (app (rest x) y)
          (merge (rest x) y))
  = {C11}
    t

Obligation 4 is proven.
        
phi_merge_perm is proven.

Lemma 6: (lorp x) /\ ~(endp x) /\ (orderedp x) => (orderedp (rest x))

Case 1:
C1. (lorp x)
C2. ~(endp x)
C3. (orderedp x)
C4. (endp (rest x))
-------------
 
    (orderedp (rest x))
  = {def of orderedp, C4}
    t

Case 1 is t.

Case 2:
C1. (lorp x)
C2. ~(endp x)
C3. (orderedp x)
C4. ~(endp (rest x))
C5. (lorp (rest x)) /\ ~(endp (rest x)) /\ (orderedp (rest x)) => (orderedp (rest (rest x)))
------------------------
C6. (orderedp x) = (if (<= (first x) (second x)) (orderedp (rest x)) nil) {def of orderedp, C2, C4}
C7. (and (<= (first x) (second x)) (orderedp (rest x))) {C6, PL}
C8. (orderedp (rest x)) {C7, PL}

Case 2 is t.

Lemma 6 is proven.

Lemma 7: (listp x) /\ (listp y) => (perm (cons e x) (cons e y)) = (perm x y)

C1. (listp x)
C2. (listp y)
----------------

LHS:(perm (cons e x) (cons e y))
  = {def of perm, def of endp, conps axiom, first-rest axiom}
    (if (in e (cons e y))
        (perm x (del e (cons e y)))
        nil)
  = {def of in, def of endp, consp axiom, first-rest axiom}
	(if (if (equal e e)
            t
            (in e y))
        (perm x (del e (cons e y)))
        nil)
  = {if axiom}
    (perm x (del e (cons e y)))
  = {def of del, def of endp, consp axiom, first-rest axiom}
    (perm x (if (equal e e) y (cons e (del e y))))
  = {if axiom}
    (perm x y}
RHS:(perm x y)

LHS = RHS, so Lemma 7 is proven


|#

:program
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; GIVEN
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
  (if (or (endp l)(endp (rest l)))
    l
    (merge (msort (first-half l 0))
           (msort (last-half l 0)))))

:logic
(check= (msort nil) nil)
(check= (msort '(1)) '(1))
(check= (msort '(2 0 17 -1/2 -42)) '(-42 -1/2 0 2 17))
(check= (msort '(-43 2 0 17 -1/2 -42)) '(-43 -42 -1/2 0 2 17))

(defthm phi_merge_order (implies (and (lorp l1)(lorp l2)(orderedp l1)(orderedp l2))
                                (orderedp (merge l1 l2))))

#|
OK.  Now let's prove (msort l) is ordered
Notice what kind of inductive assumption you'll need.

phi_msort_ordered: (lorp l) => (orderedp (msort l))

Induction Scheme
1. ~(lorp l) => phi
2. (lorp l) /\ (endp l) => phi
3. (lorp l) /\ ~(endp l) /\ (endp (rest l)) => phi
4. (lorp l) /\ ~(endp l) /\ ~(endp (rest l)) /\ phi | (l (first-half l 0)) /\ phi | (l (last-half l 0)) => phi

Obligation 1:
C1. ~(lorp l)
C2. (lorp l)
---------------
C3. f {C1, C2}

f => phi_msort_ordered is t, so Obligation 1 is proven.

Obligation 2:
C1. (lorp l)
C2. (endp l)
---------------

    (orderedp (msort l))
  = {def of msort, C2}
    (orderedp l)
  = {def of orderedp, C2}
    t

Obligation 2 is proven.

Obligation 3:
C1. (lorp l)
C2. ~(endp l)
C3. (endp (rest l))
---------------
   
    (orderedp (msort l))
  = {def of msort, C3}
    (orderedp l)
  = {def of orderedp, C3}
    t

Obligation 3 is proven.

Obligation 4:
C1. (lorp l)
C2. ~(endp l)
C3. ~(endp (rest l))
C4. (lorp (first-half l 0)) => (orderedp (msort (first-half l 0)))
C5. (lorp (last-half l 0)) => (orderedp (msort (last-half l 0)))
--------------------
C6. (lorp (first-half l 0)) {Contract of first-half}
C7. (lorp (last-half l 0)) {Contract of last-half}
C8. (orderedp (msort (first-half l 0))) {C4, C6, MP}
C9. (orderedp (msort (last-half l 0))) {C5, C7, MP}

    (orderedp (msort l))
  = {def of msort, C2, C3}
 	(orderedp (merge (msort (first-half l 0))
                     (msort (last-half l 0)))
  = {phi_merge_order | ((l1 (msort (first-half l 0))) (l2 (msort (last-half l 0))))}
    t

Oblgiation 4 is proven.

All Obligations have been proven, so phi_msort_ordered must be t.

|#


(defunc min-l (l)
  :input-contract (and (lorp l)(consp l))
  :output-contract (rationalp (min-l l))
  (if (endp (rest l))                  
    (first l)
    (let ((minrest (min-l (rest l))))
      (if (<= minrest (first l)) 
        minrest 
        (first l)))))

(check= (min-l '(1)) 1)
(check= (min-l '(1 5/2 17/2 13 -47)) -47)
(check= (min-l '(-48 5/2 17/2 13 -47)) -48)
(test? (implies (and (lorp l)(consp l))
                (in (min-l l) l)))

(sig del (all (listof :b)) => (listof :b))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ssort: lor -> lor
;; GIVEN
;; (ssort l) or selection sort takes a lorp l as input,
;; and returns a permutation of l with elements in non-decreasing order.
(defunc ssort (l)
  :input-contract (lorp l)
  :output-contract (lorp (ssort l))
  (if (endp l)
      nil
    (cons (min-l l) (ssort (del (min-l l) l)))))


#|
SECTION 3: OPEN ENDED PROBLEM (SSORT)
OK.  Now prove that ssort is ordered.
(surprisingly (perm l (ssort l)) is WAY harder than it looks which is why we aren't
doing that this assignment)

You will probably need one or more lemmata
(such as L3: (lorp l)/\(rationalp e)/\(in e l) => (<= (min-l l) e)) 

You do not need to prove L3.

Notes: 
- If you use the induction scheme for min-l, feel free to give yourself an 
inductive assumption for the ~(<= minrest (first l)) obligation as well as the 
(<= minrest (first l)) obligation.  This may seem weird but the recursion has to occur
for both of these conditions in order for the comparison to work.  Thus it is sound. 

- minrest is from a let so you can simply replace it with (min-l (rest l)).

- Oh and we may have one of those ugly "proof-by-cases within a proof" situations again.

#....Lemma stuff......#


phi_order_ssort: (lorp l) => (orderedp (ssort l))

Induction Scheme:
1. ~(lorp l) => phi
2. (lorp l) /\ (endp l) => phi
3. (lorp l) /\ ~(endp l) /\ phi | (l (del (min-l l) l)) => phi

Obligation 1:
C1. ~(lorp l)
C2. (lorp l)
-------------
C3. f {C1, C2}

f => phi_order_ssort is t, so Obligation 1 is proven

Obligation 2:
C1. (lorp l)
C2. (endp l)
-------------

    (orderedp (ssort l))
  = {def of ssort, C2}
    (orderedp nil)
  = {def of orderedp, def of endp, consp axiom}
    t

Obligation 2 is proven

Obligation 3:

Case 1:
C1. (lorp l)
C2. ~(endp l)
C3. (lorp (del (min-l l) l)) => (orderedp (ssort (del (min-l l) l)))
C4. (endp (ssort (del (min-l l) l)))
-----------------

	(orderedp (ssort l))
  = {def of ssort, C2}
    (orderedp (cons (min-l l) (ssort (del (min-l l) l))))
  = {def of orderedp, C4}
    t

Case 1 is t.

Case 2:
C1. (lorp l)
C2. ~(endp l)
C3. (lorp (del (min-l l) l)) => (orderedp (ssort (del (min-l l) l)))
C4. ~(endp (ssort (del (min-l l) l)))
-----------------
C5. (lorp (del (min-l l) l)) {L11 | (e (min-l l))}
C6. (orderedp (ssort (del (min-l l) l))) {C3, C5, MP}

	(orderedp (ssort l))
  = {def of ssort, C2}
    (orderedp (cons (min-l l) (ssort (del (min-l l) l))))
  = {def of orderedp, C4, first-rest axiom, def of second}
    (if (<= (min-l l) (first (ssort (del (min-l l) l))
        (orderedp (ssort (del (min-l l) l)))))
        nil)
  = {L9, L10, if axiom}
    (orderedp (ssort (del (min-l l) l)))
  = {C6}
    t

Case 2 is t, so Obligation 3 is proven.

All Obligations are proven, so phi_order_ssort must be t.

Lemma 8: (lorp l) /\ (consp l) => (lorp (del (min-l l) l)) 

Induction Scheme:
1. ~((lorp l) /\ (consp l)) => phi
2. (lorp l) /\ (consp l) /\ (endp (rest l)) => phi
3. (lorp l) /\ (consp l) /\ ~(endp (rest l)) /\ phi | ((l (rest l))) /\ (<= (min-l (rest l)) (first l)) => phi
4. (lorp l) /\ (consp l) /\ ~(endp (rest l)) /\ phi | ((l (rest l))) /\ ~(<= (min-l (rest l)) (first l)) => phi

Case 1:
C1. ~((lorp l) /\ (consp l))
C2. (lorp l)
C3. (consp l)
-------------
C4. f {C1, C2, C3}

f => phi is t, so Case 1 is t.

Case 2:
C1. (lorp l)
C2. (consp l)
C3. (endp (rest l))
--------------
C4. (lorp (rest l)) {C1, C2, def of lorp}

    (lorp (del (min-l l) l))
  = {def of min-l, C3}
    (lorp (del (first l) l))
  = {def of del, C2, def of endp}
    (lorp (rest l))
  = {C4}
    t

Case 2 is t.

Case 3:
C1. (lorp l)
C2. (consp l)
C3. ~(endp (rest l))
C4. (lorp (rest l)) /\ (consp (rest l)) => (lorp (del (min-l (rest l)) (rest l)))
C5. (<= (min-l (rest l)) (first l))
--------------------
C6. (lorp (rest l)) {C1, C2, def of lorp}
C7. (rationalp (first l)) {def of lorp, first-rest axiom, C1, C2}
C8. (lorp (del (min-l (rest l)) (rest l))) {C4, C6, MP}

    (lorp (del (min-l l) l))
  = {def of min-l, C3}
    (lorp (del (if (<= (min-l (rest l)) (first l))
                   (min-l (rest l))
                   (first l)) l))
  = {C5}
    (lorp (del (min-l (rest l)) l))
  = {def of del, C2}
    (lorp (if (equal (first l) (min-l (rest l)))
              (rest l)
              (cons (first l) (del (min-l (rest l)) (rest l)))
Case A: (equal (first l) (min-l (rest l)))
  = {if axiom}
    (lorp (rest l))
  = {C6}
    t
Case B: ~(equal (first l) (min-l (rest l)))
  = {if axiom}
	(lorp (cons (first l) (del (min-l (rest l)) (rest l))))
  = {def of lorp}
    (and (rationalp (first l))
         (lorp (del (min-l (rest l)) (rest l)))
  = {C7, C8, PL}
    t

Case 3 is t.

Case 4:
C1. (lorp l)
C2. (consp l)
C3. ~(endp (rest l))
C4. (lorp (rest l)) /\ (consp (rest l)) => (lorp (del (min-l (rest l)) (rest l)))
C5. ~(<= (min-l (rest l)) (first l))
-------------------
C6. (lorp (rest l)) {C1, C2, def of lorp}

    (lorp (del (min-l l) l))
  = {def of min-l C3}
    (lorp (del (if (<= (min-l (rest l)) (first l))
                   (min-l (rest l))
                   (first l)) l))
  = {C5}
    (lorp (del (first l) l))
  = {def of del, C2, first-rest axiom}
    (lorp (if (equal (first l) (first l))
              (rest l)
              (cons (first l) (del (first l) (rest l)))))
  = {if axiom}
    (lorp (rest l))
  = {C6}
    t

Case 4 is t.

All cases have been proven, so Lemma 8 is t.


Lemma 9: (lorp l) /\ ~(endp l) /\ ~(endp (ssort (del (min-l l) l))) => (in (first (ssort (del (min-l l) l)))l)

Induction Scheme:
1. ~((lorp l) /\ (consp l)) => phi
2. (lorp l) /\ (consp l) /\ (endp (rest l)) => phi
3. (lorp l) /\ (consp l) /\ ~(endp (rest l)) /\ phi | ((l (rest l))) /\ (<= (min-l (rest l)) (first l)) => phi
4. (lorp l) /\ (consp l) /\ ~(endp (rest l)) /\ phi | ((l (rest l))) /\ ~(<= (min-l (rest l)) (first l)) => phi

Case 1:
C1. ~((lorp l) /\ (consp l))
C2. (lorp l)
C3. (consp l)
-------------
C4. f {C1, C2, C3}

f => phi is t, so Case 1 is t.

Case 2:
C1. (lorp l)
C2. (consp l)
C3. ~(endp (ssort (del (min-l l) l)))
C4. (endp (rest l))
---------------
C5. (del (min-l l) l) = (rest l) {def of del, C2, C4}
C6. (ssort (rest l)) = nil {def of ssort, C5}
C7. f {C3, C6, PL}

f => phi is t, so Case 2 is t.

Case 3:
C1. (lorp l)
C2. (consp l)
C3. ~(endp (ssort (del (min-l l) l)))
C4. ~(endp (rest l))
C5. (lorp (rest l)) /\ ~(endp (rest l)) /\ ~(endp (ssort (del (min-l (rest l)) (rest l)))) => (in (first (ssort (del (min-l (rest l)) (rest l)))) (rest l))
C6. (<= (min-l (rest l)) (first l))
--------------------

    (in (first (ssort (del (min-l l) l))) l)
  = {def of min-l}
    (in (first (ssort (del (if (<= (min-l (rest l)) (first l))
                               (min-l (rest l))
                               (first l)) l))) l)
  = {C6}
    (in (first (ssort (del (min-l (rest l)) l))) l)
  = {def of del}
    (in (first (ssort (if (equal (first l) (min-l (rest l)))
                          (rest l)
                          (cons (first l) (del (min-l (rest l)) (rest l))))) l)
Case A: (equal (first l) (min-l (rest l)))
  = {if axiom}
    (in (first (ssort (rest l))) l)
  = {def of ssort}
    (in (first (if (endp (rest l))
                   nil 
                   (cons (min-l (rest l)) (ssort (rest (rest l))))))) l)
  = {C4, first-rest axiom}
    (in (min-l (rest l)) l)
  = {def of in, C2}
    (if (equal (min-l (rest l)) (first l))
        t
        (in (min-l (rest l)) (rest l)))
Case I: (equal (min-l (rest l)) (first l))
  = {if axiom}
    t
Case II: ~(equal (min-l (rest l)) (first l))
 = {if axiom}
   (in (min-l (rest l)) (rest l))
Recall Lemma 3 from HW 8: (lorp l) /\ (consp l) => (in (min-l l) l)
 = {HW8L3 | (l (rest l))}
   t
Case B: ~(equal (first l) (min-l (rest l)))
 = {if axiom}
   (in (first (ssort (cons (first l) (del (min-l (rest l)) (rest l))))) l)
 = {def of in}
   (if (equal (first (ssort (cons (first l) (del (min-l (rest l)) (rest l))))) (first l))
       t
       (in (first (ssort (cons (first l) (del (min-l (rest l)) (rest l))))) (rest l)))
Case I: (equal (first (ssort (cons (first l) (del (min-l (rest l)) (rest l))))) (first l))
 = {if axiom}
   t
Case II: ~(equal (first (ssort (cons (first l) (del (min-l (rest l)) (rest l))))) (first l))
 = {if axiom}
   (in (first (ssort (cons (first l) (del (min-l (rest l)) (rest l))))) (rest l))
 = {def of ssort, def of endp, consp axiom}
   (in (first (cons )))

 = {def of ssort, def of endp, consp axiom}
   (in (first (cons (min-l (cons (first l) (del (min-l (rest l)) (rest l))))
                    (ssort (del (min-l (cons (first l) (del (min-l (rest l)) (rest l)))) (cons (first l) (del (min-l (rest l)) (rest l))))
       l)
Let expr_a denote: (cons (first l) (del (min-l (rest l)) (rest l))) 
 = {def of in}
   (if (equal (min-l expr_a) (first l))
       t
       (in (min-l expr_a) (rest l)))
expr_a case I: (min-l expr_a) = (first l)
 = {if axiom}
   t
expr-a case II: (min-l expr_a) = (min-l (del (min-l (rest l)) (rest l)))
 = {}
   ???? --- Stuck on progress






Lemma 10: (lorp l) /\ (rationalp e) /\ (in e l) => (<= (min-l l) e) {Given}

Lemma 11: (lorp l) /\ (consp l) => (lorp (del e l))

Case 1:
C1. (lorp l)
C2. (consp l)
C3. (equal e (first l))
-------------

    (lorp (del e l))
  = {def of del, C2, C3}
    (lorp (rest l))
  = {C4}
    t

Case 1 is t.

Case 2:
C1. (lorp l)
C2. (consp l)
C3. ~(equal e (first l))
C4. (lorp (rest l)) /\ (consp (rest l)) => (lorp (del e (rest l)))
C5. (endp (rest l))
---------------------
C6. (rationalp (first l)) {C1, C2, def of lorp, first-rest axiom}

    (lorp (del e l))
  = {def of del, C2, C4}
    (lorp (cons (first l) (del e (rest l))))
  = {def of del, C5}
    (lorp (cons (first l) nil))
  = {def of lorp, C6}
    t

Case 2 is t.

Case 3:
C1. (lorp l)
C2. (consp l)
C3. ~(equal e (first l))
C4. (lorp (rest l)) /\ (consp (rest l)) => (lorp (del e (rest l)))
C5. ~(endp (rest l))
-------------------
C6. (lorp (rest l)) {C1, C2, def of lorp}
C7. (lorp (del e (rest l))) {C4, C6, C5, MP}

    (lorp (del e l))
  = {def of del, C2, C4}
    (lorp (cons (first l) (del e (rest l))))
  = {def of lorp}
    (and (rationalp (first l)) (lorp (del e (rest l))))
  = {C7, C8, PL}
    t

Case 3 is t.

All Cases are proven, so Lemma 11 is proven.




|#
```


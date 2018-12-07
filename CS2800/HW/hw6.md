```lisp
#|
CS 2800 Homework 6 - Fall 2018

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  EQUATIONAL REASONING + EXAM REVIEW
  Homework 6 is extra big because it serves not only as practice for E.R.
  but also general practice for the exam.  Thus the time it takes you is time
  spend "studying" for the exam and that practice is far more useful than reading
  the textbook again.
|#

  
#|
SECTION 1: Equational Reasoning with Propositional Structures

The following questions are meant as practice to make sure that you can 
effectively do equational reasoning on a variety of logical structures. It's 
important that you know how to use equational reasoning on these structures 
since in the future, you will have to deal with terms like these that contain 
much more complex code. This also gives you some practice with Propositional Logic.
Ideally each expression can be represented as a string of statements ANDed together which
imply a singular test: A/\B/\C=>D 
OR a group of conjectures connected with an AND: (and (A /\B/\C => D) (A /\B/\~C => E)
OR
Prove each of the following with equational reasoning or provide a counterexample:

a) (implies (and (integerp n) (integerp m) (integerp o))
            (implies (and (implies (< n 5) (> m 10)) 
                          (implies (> m 10) (> o 20)))
                     (implies (< n 1) (> o 10))))

............
C1. (integerp n)
C2. (integerp m)
C3. (integerp o)
C4. (implies (< n 5) (> m 10))
C5. (implies (> m 10) (> o 20))
-----------------

Case 1: (< n 1) is false
   false => ... is ture
Case 1 is true.

Case 2: (< n 1) is true
  (< n 1)
= {Arithmetic}
  (< n 5)
= {MP, C4}
  (> m 10)
= {MP, C5}
  (> o 20)
= {Arithmetic}
  (> o 10)
Case 2 is true.
Therefore, the conjecture is true.


b) (implies (and (integerp n) (integerp m) (integerp o))
            (and (implies (and (< n m) (> n 5))
                          (> m 5))
                 (implies (and (< m o) (< o 10)) 
                          (< m 15))))

A /\ B /\ C => ( ((D /\ E) => F) /\ ((G /\ H) => I) )
A /\ B /\ C /\ D /\ E /\ G /\ H => F /\ I

............
C1. (integerp n)
C2. (integerp m)
C3. (integerp o)
C4. (< n m) 
C5. (> n 5)
C6. (< m o)
C7. (< o 10)
----------------


C8.(> n 5)
 = {C4, Arithmetic}
   (> m 5)

C9.(< o 10)
 = {Arithmetic}
   (< o 15)
 = {C6, Arithmetic}
   (< m 15)

The conjecture must be true.

c) (implies (and (integerp n) (integerp m) (integerp o)
                 (implies (>= n m) (>= n o)))
            (>= m o))
............
A /\ B /\ C /\ (D => E) => F

The conjecture is falsifiable, with n = 10, m = 7, o = 8

d) (implies (and (integerp n) (integerp m) (integerp o))
            (or (implies (<= n m) (<= n o))
                (> m o)))

  A /\ B /\ C => ((D => E) \/ F)
= {PL}
  ~(A /\ B /\ C) \/ ((D => E) \/ F)
= {PL}
  (~(A /\ B /\ C) \/ (D => E)) \/ (~(A /\ B /\ C) \/ F)
= {PL}
  (A /\ B /\ C => (D => E)) \/ (A /\ B /\ C => F)
= {Exportation}
  (A /\ B /\ C /\ D => E) \/ (A /\ B /\ C => F)

Case 1:
C1. (integerp n)
C2. (integerp m)
C3. (integerp o)
C4. (<= n m)
C5. ~(> m o) {We're in case 1}
-------------
C6. (<= m o)

    (<= n m)
  = {C6, Arithmetic}
    (<= n o)
Case 1 is true.

Case 2:
C1. (integerp n)
C2. (integerp m)
C3. (integerp o)
C4. (<= n m)
C5. (> m o) (We're in case 2)
------------
    
    (> m o)
  = {C5}
    t
Case 2 is true.

   
                
Let's take the above conjecture and re-write it more abstractly:
A=>((B=>C)\/D)
Prove that this is equivalent to
(A/\B=>C)\/(A=>D)
...........

Now prove the new theorem.  Notice the theorem
is actually two parts.  Furthermore, if one case is (> m o)
then for the other case you can claim ~(> m o)

  A => ((B => C) \/ D)
= {a => b = ~a \/ b}
  ~A \/ ((B => C) \/ D)
= {Distributive}
  (~A \/ (B => C)) \/ (~A \/ D)
= {a => b = ~a \/ b}
  (A => (B => C)) \/ (A => D)
= {Exportation}
  (A /\ B => C) \/ (A => D)
............

e) (implies (and (booleanp a) (natp i)
                 (integerp i) (< i 0))
            (not a))
............
The conjecture is not provable, because i cannot satisfy (natp i) and (< i 0) at the same time.

A /\ B /\ C /\ D => E
false => E
the statement is always true, regardless of what E is.

f) (implies (and (natp a) (natp b) (posp d)
                 (implies (natp (/ a d))
                          (not (natp (/ b d)))))
            (implies (natp (/ a 5))
                     (not (natp (/ b 5)))))
............
A /\ B /\ C /\ (D => E) => (F => G)

C1. (natp a)
C2. (natp b)
C3. (posp d)
C4. (natp (/ a d)) => (not (natp (/ b d)))

The conjeture is false, with a = 5, b = 10. 

|#

#|
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  SECTION 2
  Theme: Annoying Teacher
  Your CS 2800 Professor really must not be much of a programmer.  He
  keeps asking you to write obviously slow code to match the "data
  definition" despite the fact there is obviously a more efficient approach. 
  Other times he simply writes functions in a weird round-about way or
  claims that two functions are the same when they are clearly not. That's not
  even counting all the times he puts a typo in an assignment (OK.  That one is
  a bit too true).
  Worse of all, he won't accept this fact when you tell him. It's your job to
  prove him wrong.

  
  Each question will be made up of several parts depending on the task:
  
  A) If you are given a conjecture that is not valid:
  1) Perform contract checking and completion
  2) Try some sample data to make sure you understand the conjecture 
  (no need to show us this)
  3) Give a counter-example
  
  B) If you need to make an improved function
  1) Write a more efficient version of the given function (if not already defined)
  2) Prove the original and the new function are equivalent (you'll want to write
  the conjecture you are proving first just to be clear)
  
  C) If given a complex propositional structure you are trying to prove.
  1) Perform contract checking and completion.
  2) Try some sample data to make sure you understand the conjecture 
  (no need to show us this)
  3) Break the conjecture down into a series of smaller parts that can be solved
  with Equational Reasoning Proofs
  4) Prove each of the sub-conjectures.  Unlike in HW05, you do not need to 
     show how each theorem is instantiated EXCEPT each time you use a definitional
     theorem (you don't need to do this normally but you should practice how
     to express a substitution). Note, some proofs (such as the one-countsp proof)
     need a lemma.  You may be required to find the lemma.
     
First, however, we need to give you a long list of rules to follow.....
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Equational Reasoning Rules:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
Below you are given a set of function definitions you can use.
The definitional axioms and contract theorems for
these functions can be used in the proof. You can use ACL2s to check
the conjectures you are proving but you are not required to do
so. 

Here are some notes about equational proofs:

1. General context: Remember to use propositional logic to rewrite
the conjectures so that it has as many hypotheses as possible.  See
the lecture notes for details. Label the facts in your
context with C1, C2, ... as in the lecture notes.

2. The derived context: Draw a dashed line (----) after the general context 
and add anything interesting that can be derived from the context.  
Use the same labeling scheme as was used in the context. Each derived
fact needs a justification. Again, look at the lecture notes for
more information.

3. Use the proof format shown in class and in the lecture notes,
which requires that you justify each step.  Explicitly name each
axiom, theorem or lemma you use, although we are starting to "cut 
corners" with the if axiom and the instantiations we are using. You
can take any "shortcuts" discussed in class.

5. When using the definitional axiom of a function, the
justification should say "Def. <function-name>".  When using the
contract theorem of a function, the justification should say
"Contract <function-name>". For definitional axioms alone, you should
write the instantiation (for practice)

6. Arithmetic facts such as commutativity of addition can be
used. The name for such facts is "arithmetic".

7. You can refer to the axioms for cons, and consp as the "cons axioms", 
Axioms for first and rest can be referred to as "first-rest axioms".
The axioms for if are named "if axioms"

8. Any propositional reasoning used can be justified by "propositional
reasoning", "Prop logic", or "PL", except you should use "MP" 
to explicitly identify when you use modus ponens and MT for Modus Tollens.

9. For any given propositional expression, feel free to re-write it
in infix notation (ex a =>(B/\C)).

10. For this homework, you can only use theorems we explicitly
tell you you can use or we have covered in class / lab. 
You can, of course, use the definitional axiom and contract 
theorem for any function used or defined in this homework. You
may also use theorems you've proven earlier in the homework.
Here are the definitions used for the remainder of the questions.

;; listp is built in but for this assignment, assume it 
;; is defined this way
(defunc listp (l)
  :input-contract t
  :output-contract (booleanp (listp l))
  (if (consp l)
     (listp (rest l))
     (equal l nil)))
    
(defunc endp (l)
  :input-contract (listp l)
  :output-contract (booleanp (endp l))
  (if (consp l) nil t))
  
(defunc len (x)
  :input-contract (listp x)
  :output-contract (natp (len x))
  (if (endp x)
    0
    (+ 1 (len (rest x)))))

(defunc app (x y)
  :input-contract (and (listp x)(listp y))
  :output-contract (listp (app x y))
  (if (endp x)
    y
    (cons (first x)(app (rest x) y))))

(defunc in (a l)
  :input-contract (listp l)
  :output-contract (booleanp (in a l))
  (if (endp l)
    nil
    (or (equal a (first l)) (in a (rest l)))))

|# 
;; delete : All x List -> List
;; removes all instances of the element a from the list
(defunc delete (a l)
  :input-contract (listp l)
  :output-contract (listp (delete a l))
  (cond ((endp l) nil)
        ((equal a (first l)) (delete a (rest l)))
        (t (cons (first l) (delete a (rest l))))))

;; duplicate: List -> List
;; repeats each element in the given list once
(defunc duplicate (l)
  :input-contract (listp l)
  :output-contract (listp (duplicate l))
  (if (endp l)
    nil
    (cons (first l) (cons (first l) (duplicate (rest l))))))

(defunc snoc (e l)
  :input-contract (listp l)
  :output-contract (listp (snoc e l))
  (if (endp l)
    (list e)
    (cons (first l)(snoc e (rest l)))))

(sig delete (all (listof :b)) => (listof :b))

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
                            (delete (first l1) l2))))
          (t (intersect (rest l1) l2))))

(defdata factor (range integer (2 <= _)))

;; Ignore
(defthm phi_shrink_factor (implies (and (posp n)(factorp f)(posp (/ n f)))
                                   (< (/ n f) n)))

(defdata lop (listof pos))
(defdata lor (listof rational))

;; factors-help : Pos x factor -> Lop
;; returns a list containing all PRIME factors of n including 
;; (possibly) n and 1.
(defunc factors-help (n p)
  :input-contract (and (posp n) (factorp p))
  :output-contract (lopp (factors-help n p))
  (cond ((> p n)  nil)
        ((posp (/ n p)) (cons p (factors-help (/ n p) p)))
        (t              (factors-help n (+ p 1)))))

;; factors : Pos -> Lop
;; returns a list containing all PRIME factors of n including (possibly) 
;; n. In all cases, 1 is a factor but is left out for simplicity sake. 
(defunc factors (n)
    :input-contract (posp n)
    :output-contract (lopp (factors n))
    (let ((fh (factors-help n 2)))
      (if (equal n 1)
        (list 1)
        (if (endp fh) (list 1 n) 
          (cons 1 fh)))))

;; this line is for ACL2s (you can ignore it)
(sig intersect ((listof :b) (listof :b)) => (listof :b))

;; prod-list: lop -> Pos
;; (prod-list lp) multiplies all values in list lp together
;; If the list is emtpy, return 1.
(defunc prod-list (lp)
  :input-contract (lopp lp)
  :output-contract (posp (prod-list lp))
  (if (endp lp)
    1
    (* (first lp)(prod-list (rest lp)))))

;; NOTE: a let is the same as what f1 is equivalent to.  Thus
;; everywhere you see f1 you can plug in (factors (first lp)) when
;; doing your proofs.
(defunc factor-list (lp)
  :input-contract (lopp lp)
  :output-contract (lopp (factor-list lp))
  (if (endp lp)
    nil
    (let ((f1 (factors (first lp))))
      (if (endp (rest lp))
        f1
        (intersect f1 (factor-list (rest lp)))))))

;; gcd-list: lop -> pos
;; (gcd-list lp) determines the greatest common divisor for 
;; the entire list of positive integers lp
(defunc gcd-list (lp)
    :input-contract (lopp lp)
    :output-contract (posp (gcd-list lp))
    (prod-list (factor-list lp)))

#|
Q1.  Remember the terrible first approach to f2l (first to last) from lecture?
Your instructor, who shall remain nameless, initially wrote snoc (cons backwards or attaching
an element to the end of a list rather than the start) and then kept calling
snoc to rotate the list.  The real solution we went over uses app.

Prove the following conjecture to prove your instructor really didn't make the
smartest design decision (app is a better more general solution after all):
(and (implies (not (consp l)) 
              (equal (snoc e l)(app l (list e))))
     (implies (and (consp l) 
                   (implies (listp (rest l))
                            (equal (snoc e (rest l)) (app (rest l)(list e)))))
              (equal (snoc e l)(app l (list e)))))
.............
Case 1:
C1. ~(consp l)
C2. (listp l)
------------
C3. (endp l) {C1, C2, Def of endp, consp axiom}

LHS:(snoc e l)
  = {Def of snoc, C3, if axiom}
    (list e)
RHS:(app l (list e))
  = {Def of app, C3, if axiom}
	(list e)
LHS = RHS, so Case 1 is t.

Case 2:
C1. (consp l)
C2. (listp (rest l)) => (snoc e (rest l)) = (app (rest l) (list e))
C3. (listp l) {Contract of Snoc}
-------------
C4. (listp (rest l)) {Consp axiom, C1, C3, first-rest axiom}
C5. (snoc e (rest l)) = (app (rest l) (list e)) {C2, C4, MP}
C6. ~(endp l) {Def of Endp, C1}

LHS:(snoc e l)
  = {Def of snoc, C6, if axiom}
    (cons (first l) (snoc e (rest l)))
  = {C5}
    (cons (first l) (app (rest l) (list e)))
RHS:(app l (list e)
  = {Def of app, C6, if axiom}
    (cons (first l) (app (rest l) (list e)))
LHS = RHS, so Case 2 is t.

Both Case 1 and Case 2 is t, so the conjecture is t.
|#

#|
Q2 
Notice the factors function above (from HW03).  Well once again your instructor 
made some inefficient code.  In his version he kept getting the GCD rather than
getting the intersect of the factors and then returning the GCD at the very end.
Here is his code:
|#
(defunc gcd (n m)
  :input-contract (and (posp n)(posp m))
  :output-contract (posp (gcd n m))
  (prod-list (intersect (factors n)(factors m))))


;; gcd-list2: lopp -> pos
;; (gcd-list2 lp) calculates the common GCD for numbers in lp
;; by getting the GCD between the first element and the GCD of the
;; rest of the list. gcd-list2 should equal gcd-list
(defunc gcd-list2 (lp)
    :input-contract (lopp lp)
    :output-contract (posp (gcd-list2 lp))
    (if (endp lp)
      1
      (if (endp (rest lp))
        (first lp)
        (gcd (first lp)(gcd-list2 (rest lp))))))

;; In case you don't believe it's true, here is ACL2s showing the equality
(test? (implies (lopp lp) (equal (gcd-list2 lp)(gcd-list lp)))) 

#|
Prove the following conjecture.  Notice this is a LEMMA on the way to prove gcd-list = gcd-list2:
phi_phi_gl_fl: 
(implies (or (endp lp)
             (and (not (endp lp))(endp (rest lp)))
             (and (not (endp lp))
                  (implies (lopp (rest lp))(equal (gcd-list2 (rest lp))(prod-list (factor-list (rest lp)))))))
         (equal (gcd-list2 lp)(prod-list (factor-list lp)))

These theorems could be useful (you can use it without proving it):
phi_pl_factors: (lopp lp) => ((factors (prod-list lp))))) = lp)
phi_factors_pl: (posp p) => ((prod-list (factors p)) = p)
         
Note you may want to re-arrange phi_phi_gl_fl to get something of the form
(and (implies (and A B C) D)
     (implies (and A B (and (not C) E)) D)
     (implies (and A B (not C) E) D))
You would then prove each part of the "and" as a separate proof 
Prove phi_gl_fl

(and (implies (endp lp) (equal (gcd-list2 lp) (prod-list (factor-list lp))))
     (implies (and (not (endp lp)) (endp (rest lp)))
 	          (equal (gcd-list2 lp)(prod-list (factor-list lp)))
     (implies (and (not (endp lp))
                   (implies (lopp (rest lp)) (equal (gcd-list2 (rest lp))(prod-list (factor-list (rest lp))))))
              (equal (gcd-list2 lp)(prod-list (factor-list lp))))

Case 1:
C1. (endp lp)
C2. (lopp lp) {Contract of gcd-list2}
------------

LHS:(gcd-list2 lp)
  = {Def of gcd-list2, C1, if axiom}
    1
RHS:(prod-list (factor-list lp))
  = {Def of factor-list, C1, if axiom}
    (prod-list nil)
  = {Def of prod-list, Def of endp, consp axiom}
    1
LHS = RHS, so Case 1 is t.

Case 2:
C1. ~(endp lp)
C2. (endp (rest lp))
C3. (lopp lp) {Contract of gcd-list2}
-------------

LHS:(gcd-list2 lp)
  = {Def of gcd-list2, C1, C2, if axiom}
    (first lp)
RHS:(prod-list (factor-list lp))
  = {Def of factor-list, C1, C2, if axiom}
    (prod-list (factors (first lp)))
  = {phi_factors_p1 | ((p (first lp))), C3}
    (first lp)
LHS = RHS, so Case 2 is t.

Case 3:
C1. ~(endp lp)
C2. ~(endp (rest lp))
C3. (lopp lp) {Contract of gcd-list2}
C4. (lopp (rest lp)) => (gcd-list2 (rest lp)) = (prod-list (factor-list (rest lp)))
---------------------
C5. (consp (rest lp)) {C2, Def of endp}
C6. (lopp (rest lp)) {C3, first-rest axiom}
C7. (gcd-list2 (rest lp)) = (prod-list (factor-list (rest lp)))
C8. (lopp (factor-list (rest lp))) {C6, Contract of factor-list}

LHS:(gcd-list2 lp)
  = {Def of gcd-list2, C1, C2, if axiom}
    (gcd (first lp) (gcd-list2 (rest lp)))
  = {Def of gcd}
    (prod-list (intersect (factors (first lp)) (factors (gcd-list2 (rest lp)))))
  = {C4}
    (prod-list (intersect (factors (first lp)) (factors (prod-list (factor-list (rest lp))))))
  = {phi_pl_factors, C8}
    (prod-list (intersect (factors (first lp)) (factor-list (rest lp))))
RHS:(prod-list (factor-list lp))
  = {Def of factor-list, C1, C2, if axiom}
    (prod-list (intersect (factors (first lp)) (factor-list (rest lp))))
LHS = RHS, so Case 3 is t.

Case 1, Case 2, and Case 3 is t, so the conjecture must be t.

...............
  
  Next, prove the main conjecture
  (lopp lp) => ((gcd-list lp) = (gcd-list2 lp))
  
...............
C1. (lopp lp)
C2. (lopp lp) => (gcd-list2 lp) = (prod-list (factor-list lp)) {phi_phi_gl_fl}
---------------
C3. (gcd-list2 lp) = (prod-list (factor-list lp))

LHS:(gcd-list lp)
  = {Def of gcd-list}
    (prod-list (factor-list lp))
  = {C3}
    (gcd-list2 lp)
RHS:(gcd-list2 lp)

LHS = RHS, so the conjecture must be true.
|#

#|
QUESTION 3 IS UNGRADED AND LEFT FOR EXTRA PRACTICE IF YOU WANT IT.
;; Question #3: Duplicate Length
|#

;; GIVEN:
;; evenp : Nat -> Boolean
;; evenp is a predicate that takes a natural number and decides 
;; whether it is even.
(defunc evenp (x)
    :input-contract (natp x)
    :output-contract (booleanp (evenp x))
    (cond ((equal x 0) t)
          ((equal x 1) nil)
          (t (evenp (- x 2)))))

#|
Consider the following true conjecture:  
dup_even_len: (implies (listp l) (evenp (len (duplicate l))))

If you try to prove this using just equational reasoning, it's not
going to go well. If you're not sure why, feel free to try it. To make this
problem a bit easier, we're going to break it down into cases and give you a
little extra help.

a) Let's start with a simple case. What about when l is not a list? Prove the
following using equational reasoning:

(implies (not (listp l))
    (implies (listp l) (evenp (len (duplicate l)))))
...............

b) Next we have the base case where l is the empty list. Prove the following
using equational reasoning:

(implies (and (listp l) (endp l))
         (implies (listp l) (evenp (len (duplicate l)))))

...............

c) Those cases weren't so bad. Here comes the trickiest one, the case for 
non-empty lists. This one is hard, so we're going to give you an extra premise 
to help out. Prove the following using equational reasoning:

(implies (and (listp l) 
              (not (endp l))
              (implies (listp (rest l)) 
                       (evenp (len (duplicate (rest l))))))
         (implies (listp l) (evenp (len (duplicate l)))))

...............
|#


;; Question #4: Unduplicate

;; a) Write the function every-other:

;; every-other : EvenList -> List
;; even-elems takes a list of even length and returns  
;; a list containing every second element
(defunc every-other (l)
    :input-contract (and (listp l) (evenp (len l)))
    :output-contract (listp (every-other l))
    (if (endp l)
        nil
        (cons (first l) (every-other (rest (rest l))))))



#|
b)
You may notice that this is sort of the opposite of duplicate. In fact, the
following conjecture should be true:

phi_eo_dup:
(implies (listp l) (equal (every-other (duplicate l)) l))

Your instructor is claiming your code for every-other is wrong.  The only way to prove that
you coded that correctly is to prove phi_eo_dup

Similar to the last problem, we're going to break our proof down into cases.
Prove the following conjecture:

(and (implies (not (listp l))
              (implies (listp l) (equal (every-other (duplicate l)) l)))
     (implies (and (listp l) (endp l))
              (implies (listp l) (equal (every-other (duplicate l)) l)))
     (implies (and (listp l) 
                   (not (endp l))
                   (implies (listp (rest l)) 
                            (equal (every-other (duplicate (rest l))) 
                                   (rest l))))
              (implies (listp l) (equal (every-other (duplicate l)) l))))

...............
Case 1:
C1. ~(listp l)
--------------

  (listp l) => (every-other (duplicate l)) = l
= {C1}
  f => (every-other (duplicate l)) = l
= {PL}
  t

Case 1 is t.

Case 2:
C1. (listp l)
C2. (endp l)
-----------
C3. l = nil {C2, Def of endp, consp axiom}

LHS:(every-other (duplicate l))
  = {Def of duplicate, C2, if axiom}
    (every-other nil)
  = {Def of every-other, Def of endp, consp axiom, if axiom}
    nil
RHS:l
  = {C3}
    nil

LHS = RHS, so Case 2 is t.

Case 3:
C1. ~(endp l)
C2. (listp l)
C3. (listp (rest l)) => (every-other (duplicate (rest l))) = (rest l)
-------------
C4. (consp l) {C1, Def of endp}
C5. (listp (rest l)) {C4, Def of consp}
C6. (every-other (duplicate (rest l))) = (rest l) {C5, C3, MP}
C7. (rest (rest (cons (first l) (cons (first l) (duplicate (rest l)))))) = (duplicate (rest l)) {first-rest axiom}

LHS:(every-other (duplicate l))
  = {Def of duplicate, if axiom}
    (every-other (cons (first l) (cons (first l) (duplicate (rest l)))))
  = {Def of every-other, if axiom}
    (cons (first l) (every-other (rest (rest (cons (first l) (cons (first l) (duplicate (rest l))))))))
  = {C7}
	(cons (first l) (every-other (duplicate (rest l))))
  = {C6}
    (cons (first l) (rest l))
  = {Def of cons}
    l
RHS:l

LHS = RHS, so Case 3 is t.

Case 1, Case 2, and Case 3 are t, so the conjecture must be true.

c) OK.  So you think you're so smart.  Your instructor wants you to prove the following conjecture to
show that every-other was implemented correctly:
phi_dup_eo: (implies (listp l)(equal l (duplicate (every-other l))))

...............

The conjecture is falsifiable:
l = (list 1 2 3 4)
(every-other l) = (list 1 3)
(duplicate (every-other l)) = (list 1 1 3 3)
(list 1 1 3 3) =/= l

|#


#|
SECTION 3: Error fixes 0 points: for practice
Without looking at the code, identify 3 errors in function update
1) Write the line of code that is wrong
2) Identify the error type (infinite loop, body violation, contract violation)
3) Propose a fix for the error
(stolen from HW04)

(defdata UnaryOp '~)
(defdata BinaryOp (enum '(^ v <> =>)))

(defdata PXVar (enum '(a b c d e f g h i j k p q r s x y z)))

; PropEx: A Propositional Expression (PropEx) can be a boolean (t
; or nil), a PXVar denoting a variable (e.g. 'p or 'q), or a
; list denoting a unary or binary operation. 
(defdata (BinEx (list BinaryOp PropEx PropEx))
  (UnaryEx (list UnaryOp PropEx))
  (PropEx (oneof boolean PXVar UnaryEx BinEx)))

(defunc update (px name val)
  :input-contract (and (PropExp px) (PXVarp name) (booleanp val))
  :output-contract (Propexp (update px name val))
  (cond ((booleanp px) (update px name val))
        ((PXVarp px)
         (if (equal px name) val px))
        ((UnaryExp px)
         (list '~ (update (rest px) name val)))
        (t (cons (first px)
                 (cons (update (second px) name val)
                       (update (third px) name val))))))

1) Line
   Error
   Fix
2) Line
   Error Type
   Fix
3) Line
   Error Type
   Fix
   
|#

#|
SECTION 4 (DOES count for points)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
PROPOSITIONAL LOGIC

We use the following ascii character combinations to represent the Boolean
connectives:

  NOT     ~

  AND     /\
  OR      \/ 

  IMPLIES =>

  EQUIV   =
  XOR     <>

The binding powers of these functions are listed from highest to lowest
in the above table. Within one group (no blank line), the binding powers
are equal. This is the same as in class.


Simplify each of the following propositional expressions using 
logical equivalences like we have done in previous assignments 
and categorize each expression.  If valid or unsatisfiable, 
you must prove this us by showing the expression is equal
to t or nil.  If Satisfiable and Falsifiable, then give
a substitution (in the form Exp|((a1 i1)(a2 i2)....)) which
makes the expression false and another that makes it true.
Show the expression immediately after the substitution.

For example, the following 3 expressions would be grouped as follows:
EX1) a /\ (b \/ a) 
     = {Absorption}
     a
     Satisfiable and falsifiable
     a | ((a t)) = t
     a | ((a nil)) = nil
     
EX2) (a /\ T) /\ ~a
     = {Associativity, Commutative}
     (a /\ ~a /\ T)
     = {(p /\ nil) = nil, p/\~p = nil}
     nil
Thus EX2 is unsatisfiable.
 
1) a \/ ( p => ~p ) \/ ( b /\ a )
...........
 = {a => b = ~a \/ b | ((a p) (b ~p))}
   a \/ (~p \/ ~p) \/ (b /\ a)
 = {a \/ a = a | ((a ~p))}
   a \/ ~p \/ (b /\ a)
 = {commutative, Distributive}
   ~p \/ ((~a \/ b) /\ (~a \/ a))
 = {~a \/ a = t}
   ~p \/ ((~a \/ b) /\ t)
 = {Identity, Associativity}
   ~p \/ ~a \/ b
The expressions is satisfiable and falsifiable
   ~p \/ ~a \/ b | ((p t) (a t) (b nil)) = nil
   ~p \/ ~a \/ b | ((p f) (a t) (b nil)) = t
	
	
2) ( b = b ) /\ (( b <> b ) \/ a)
...........
 = {Distributive}
   ((b = b) /\ (b <> b)) \/ ((b = b) /\ a)
 = {b = b = t, b <> b = nil}
   (t /\ nil) \/ (t /\ a)
 = {t /\ nil = nil}
   nil \/ (t /\ a)
 = {Identity}
   a
The expression is satisfiable and falsifiable
a | ((a t)) = t
a | ((a nil)) = nil


3) p /\ ~a  => nil
..............
= {a => b = ~a \/ b | ((a (p /\ ~a)) (b nil))}
  ~(p /\ ~a) \/ nil
= {Identity}
  ~(p /\ ~a)
= {DeMorgans}
  ~p \/ a
The expression is satisfiable and falsifiable
~p \/ a | ((p t) (a nil)) = nil
~p \/ a | ((p t) (a t)) = t


4) ( ~p \/ a ) /\ ( ~a /\ p ) => ( a <> ~a )
........
 = {a <> b = ~(a = b) | ((b ~a))}
   (~p \/ a) /\ (~a /\ p) => ~(a = ~a)
 = {a = ~a = nil, ~nil = t}
   (~p \/ a) /\ (~a /\ p) => t
 = {Distributive}
   (~p /\ (~a /\ p)) \/ (a /\ (~a /\ p)) => t
 = {Commutative, Associative}
   ((~p /\ p) /\ ~a) \/ ((a /\ ~a) /\ p) => t
 = {a /\ ~a = nil}
   (nil /\ ~a) \/ (nil /\ p) => t
 = {Identity}
   nil \/ nil => t
 = {nil \/ nil = nil}
   nil => t
 = {nil => ... is t}
   t
The expression is valid.

|#

#|
SECTION 5 (again, this section IS worth points)
 Rock-Paper-Scissors-Lizard-spocK (RPSLK) and Beyond
 The classic game Rock-Paper-Scissors or Roshambo 
 (https://en.wikipedia.org/wiki/Rock%E2%80%93paper%E2%80%93scissors) 
 is a 2 player game where each person simulaneously makes a hand sign
 (indicating rock paper or scissors) and each sign either beats or is
 beaten by another. This game can be extended to any odd number of 
 hand signs provided there are an equal number of ways to 
 win and lose against each sign. Hence Dr. Sheldon Cooper from
 the Big Bang Theory suggests Rock-Paper-Scissors-Lizard-Spock (the K 
 for Spock is so we don't have a Scissors and Spock collission and 
 you can write your move with a single character rather than writing 
 words each time)
 (http://bigbangtheory.wikia.com/wiki/Rock_Paper_Scissors_Lizard_Spock)
 FYI: The extended game predates the show. I just like pop culture references
 
 The rules are as follows:
  Scissors cuts Paper
  Paper covers Rock
  Rock crushes Lizard
  Lizard poisons Spock
  Spock smashes Scissors
  Scissors decapitates Lizard
  Lizard eats Paper
  Paper disproves Spock
  Spock vaporizes Rock
  (and as it always has) Rock crushes Scissors
  
 Since we know many of you use a divide and conquer approach to splitting up
 your homework (despite us telling you not to) we figured we would give you
 a tool to decide which partner does the hard problems.  Each player will define
 a list of RPSLK decisions and the function you write below will run a series
 of RPSLK matches to determine who won the most in the series. A single match just
 seems way to easy.
 
 Instructions For Playing Game:
 1) Player 1: Fill in the *p1* constant with as many signs / matches
    as desired.  Player 2 should not look at this.
 2) Player 1: Scroll down to the *p2* definition below.
 3) Player 1: Now close your eyes (or walk away from the screen)
 4) Player 2: Change the list of signs in the *p2* definition. You can 
    have as many as you want.
 4b) Player 2: Change player 1's selections without him or her looking.
     Pretend you didn't do this
 5) Player 1 can open his or her eyes.
 6) Admit all RPSLK code for Part 1.~p \/ a | ((p t) (a nil)) = nil

 
 7) From the Read-Evaluate-Print Loop call 
 (runMatches *p1* *p2* *RPSLK-rules*)
 or 
 (runMatchesMsg *p1* *p2* *RPSLK-rules*)

 8) Player 2: Claim you won. Optional: give an evil laugh.
|#

;; Data Definitions
;; RPSLK is the set of possible signs one can make and a loc is a list
;; or series of matches that use these signs.
(defdata RPSLK (enum '(R P S L K)))
(defdata loc (listof RPSLK))

(defconst *p1* (list 'S 'S 'S 'S 'S))

;; Define a record to describe win/loss cases. The defconst below 
;; should work using your record:
(defdata RPSLK-rule (record (winner . RPSLK)
                            (loser . RPSLK)
                            (statement . string)))

(defconst *SP-rule* (RPSLK-rule 'S 'P "Scissors cuts Paper"))
(defconst *PR-rule* (RPSLK-rule 'P 'R "Paper covers Rock"))
(defconst *RL-rule* (RPSLK-rule 'R 'L "Rock crushes Lizard"))
(defconst *LK-rule* (RPSLK-rule 'L 'K "Lizard poisons Spock"))
(defconst *KS-rule* (RPSLK-rule 'K 'S "Spock crushes Scissors"))
(defconst *SL-rule* (RPSLK-rule 'S 'L "Scissors decapitates Lizard"))
(defconst *LP-rule* (RPSLK-rule 'L 'P "Lizard eats Paper"))
(defconst *PK-rule* (RPSLK-rule 'P 'K "Paper disproves Spock"))
(defconst *KR-rule* (RPSLK-rule 'K 'R "Spock vaporizes Rock"))
(defconst *RS-rule* (RPSLK-rule 'R 'S "Rock crushes Scissors"))

;; Now bundle these rules in whatever way you think would be most effective
;; so that all the game rules can be passed as a single rule-list parameter
(defdata rule-list (listof RPSLK-rule))
(defconst *RPSLK-rules*
  (list *SP-rule* *PR-rule* *RL-rule* 
        *LK-rule* *KS-rule* *SL-rule* 
        *LP-rule* *PK-rule* *KR-rule*
        *RS-rule*))

;; Define an integer range between -1 and 1 (inclusive) for getWinner values
(defdata winscore (range integer (-1 <= _ <= 1)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; DEFINE
;; getWinner: RPSLK x RPSLK x rule-list -> WinScore
;; (getWinner p1 p2 rules) takes player 1's sign p1
;; player 2's sign p2, and the game rules (rules)
;; and returns -1 if p2 wins, 1 if p1 wins and 0 for a tie
(defunc getWinner (p1 p2 rules)
    :input-contract (and (RPSLKp p1)(RPSLKp p2)(rule-listp rules))
  	:output-contract (winscorep (getWinner p1 p2 rules))
    (if (endp rules)
        0
	  	(let* ((f (first rules))
               (win (RPSLK-rule-winner f))
         	   (lose (RPSLK-rule-loser f)))
              (cond
                  ((and (equal p1 win) (equal p2 lose))  1)
            	  ((and (equal p2 win) (equal p1 lose)) -1)
            	  (t          (getWinner p1 p2 (rest rules)))))))



(check= (getWinner 'S 'P *RPSLK-rules*) 1)
(check= (getWinner 'P 'S *RPSLK-rules*) -1)
(check= (getWinner 'S 'S *RPSLK-rules*) 0)
(check= (getWinner 'R 'P *RPSLK-rules*) -1)

;; Ignore
(defthm phi_addwin (implies (and (winscorep r1)(integerp i))
                            (integerp (+ r1 i))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; DEFINE
;; runMatches: loc x loc x rule-list -> Integer
;; (runMatches p1List p2List rules) goes through
;; pairs of symbols from the list of choices given
;; by both players.  The game stops when one
;; player has given no more choices. Add each outcome
;; to the final tally. A positive values means player 1
;; wins, negative means player 2 wins, and 0 indicates a tie.
(defunc runMatches (p1List p2List rules)
    :input-contract (and (locp p1List) (locp p2List) (rule-listp rules))
    :output-contract (integerp (runMatches p1List p2List rules))
    (if (or (endp p1List) (endp p2List))
        0
        (+ (getWinner (first p1List) (first p2List) rules)
           (runMatches (rest p1List) (rest p2List) rules))))

;; PLAYER 2
(defconst *p2* (list 'R 'P 'S 'L 'K))

(check= (runMatches *p1* *p2* *RPSLK-rules*) 0)
;; Add extra tests.
(check= (runMatches nil (list 'R) *RPSLK-rules*) 0)
(check= (runMatches (list 'S) (list 'R) *RPSLK-rules*) -1)
(check= (runMatches (list 'R) (list 'P 'S) *RPSLK-rules*) -1)
(check= (runMatches (list 'R 'S) (list 'S 'L) *RPSLK-rules*) 2)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Let's make things prettier and easier to follow. Write a
; function that runs the matches but records the person who won
; each time and the text message associated with the match.  
; For example (runMatchesMsg '(K K) '(L R) *RPSLK-rules*)
; returns (LIST (LIST 'P2 "Lizard poisons Spock")
;               (LIST 'P1 "Spock vaporizes Rock"))
; Ties should output information indicating what both players chose
; such as (LIST '- 'K)... 'K was used just to make your life easier 
; so you don't have to generate strings.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; DEFINE
;; getWinMsg: RPSLK x RPSLK x rule-list -> String
;; (getWinMsg p1 p2 rules) takes the sign from player 1 and 2
;; plus the game rules and outputs a string message indicating
;; the outcome (such as "Lizard poisons Spock"). 
;; If no rule is found, "No match" can be returned.
(defunc getWinMsg (p1 p2 rules)
    :input-contract (and (RPSLKp p1)(RPSLKp p2)(rule-listp rules))
 	:output-contract (COMMON-LISP::STRINGP (getWinMsg p1 p2 rules))
  	(if (endp rules)
        "No match"
	  	(let* ((f (first rules))
               (win (RPSLK-rule-winner f))
         	   (lose (RPSLK-rule-loser f))
               (msg (RPSLK-rule-statement f)))
              (cond
                  ((and (equal p1 win) (equal p2 lose)) msg)
            	  ((and (equal p2 win) (equal p1 lose)) msg)
            	  (t           (getWinMsg p1 p2 (rest rules)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; DEFINE
;; genMatchMsg: RPSLK x RPSLK x rule-list -> List
;; (genMatchMsg p1 p2 rules) make a list of 2 elements,
;; typically the winner's symbol ('P1 or 'P2) plus
;; the rule string.
(defunc genMatchMsg (p1 p2 rules)
    :input-contract (and (RPSLKp p1)(RPSLKp p2)(rule-listp rules))
     :output-contract (listp (genMatchMsg p1 p2 rules))
    (if (endp rules)
        (list p1 "No match")
        (let* ((f (first rules))
               (win (RPSLK-rule-winner f))
                (lose (RPSLK-rule-loser f))
               (msg (RPSLK-rule-statement f)))
              (cond
                  ((and (equal p1 win) (equal p2 lose)) (list p1 msg))
                  ((and (equal p2 win) (equal p1 lose)) (list p2 msg))
                  (t                  (genMatchMsg p1 p2 (rest rules)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; runMatchesMsg: loc x loc x rule-list -> List
;; (runMatchesMsg p1List p2List rules) goes through
;; pairs of symbols from the list of choices given
;; by both players. The game stops when one
;; player has given no more choices. The output list
;; consists of all match messages.
(defunc runMatchesMsg (p1List p2List rules)
  :input-contract (and (locp p1List)
                       (locp p2List)
                       (rule-listp rules))
  :output-contract (listp (runMatchesMsg p1List p2List rules))
  (if (or (endp p1List)(endp p2List))
    nil
    (cons (genMatchMsg (first p1List)(first p2List) rules)
          (runMatchesMsg (rest p1List)(rest p2List) rules))))

(runMatchesMsg *p1* *p2* *RPSLK-rules*)

;; Run more tests.
(check= (runMatchesMsg nil nil *RPSLK-rules*) nil)
(check= (runMatchesMsg '(R P) '(S P) *RPSLK-rules*) 
        (list '(R "Rock crushes Scissors")
              '(P "No match")))

;; GOOD LUCK ON THE EXAM

```


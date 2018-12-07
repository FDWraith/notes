```lisp
#|
CS 2800 Homework 5 - Fall 2018

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
   this file.  There will be a 10 point penalty for this UNLESS you rename
   the file extension to simply be .txt  This is going to be our hacky work-around
   to the MAC OS issue for the time being.

 * You must list the names of ALL group members below, using the given
   format. This way we can confirm group membership with the BB groups.
   If you fail to follow these instructions, it costs us time and
   it will cost you points, so please read carefully.


Names of ALL group members: Kevin Zhang, Jemin Park

Note: There will be a 10 pt penalty if your names do not follow 
this format.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

For this homework you will NOT need to use ACL2s. I typically have you prove
things and have ACL2s prove things but in this case, due to the MAC OS issue,
I'm making this purely text editing.

Technical instructions (provided you don't use a text editor):

- open this file in ACL2s as hw05.lisp.

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
  can see what the syntax is. This isn't a big deal for this assignment.  Most
  work will be done in comments.

- when done, save your file and submit it as hw05.lisp

- avoid submitting the session file (which shows your interaction with the
  theorem prover). This is not part of your solution. Only submit the lisp
  file!
|#
#|
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
Section 1: SUBSTITUTIONS

Below you are given a set of ACL2s terms and substitutions. Recall
that a substitution is a list of pairs, where the first
element of each pair is a variable (or atom) and the second an
expression (or image). Also, variables can appear only once as a left
element of a pair in any substitution. For example, the
substitution ((y (cons a b)) (x m)) maps y to (cons a b) and x to
m. 

For each term/substitution pair below, show what you get when
you apply the substitution to the term (i.e., when you
instantiate the term using the given substitution).  
If the substitution is syntactically invalid (not allowed), indicate why.
Note: you do not need to evaluate the returned expression.  Also note
(app x) is perfectly acceptable despite app taking two arguments. Substitution
is purely a variable substitution operation. It doesn't take into account meaning.

Example.  (foo (bar x y) z)|
          ((x (list 3))(y (cons 'l nil))(z (+ 1 2)))
          *(foo (bar (list 3) (cons 'l nil)) (+ 1 2))
(The * simply indicates the answer line)

In class we wrote this as 
(foo (bar x y) z)| ((x (list 3))(y (cons 'l nil))(z (+ 1 2))) but
the two line format will make it easier for you to read.
          
a. (app z (rev (cons w y)) z)|
   ((w 1) (y (app a b)) (z (rev a)))
   ........
   (app (rev a) 
        (rev (cons l (app a b))) 
        (rev a))
   
b. (app (cons 'w x) x)|
   ((w (cons a (list d))) (x (cons x nil)))
   ........
   (app (cons 'w (cons x nil)) 
        (cons x nil))

c. (* (+ x (/ y (len z))) (+ (len z) y))
   ((x (+ x y))(+ -) (y (/ x y)) (z '(3 4)))
   ........
   (* (- (+ x y) (/ (/ x y) (len '(3 4)))) 
      (- (len '(3 4)) (/ x y)))
   
d. (implies (endp u) (equal (len (app u w))(len w)))
   ((u w)(w u))
   ........
   (implies (endp w) (equal (len (app w u))(len u)))


e. (equal (+ (+ (len x) (len y)) (len z)) (len (cons 'z (app 'x y))))
   ((x '(5 6)) (x '(a b)) (y '(2 8)) (z '(17 3)))
   ........
   Not valid substitution because x is repeated, and we would not know which 
   x to substitute in.

f. (cons u (app u w))|
   ((u (app w w))(w (app u u)))
   ........
   (cons (app w w) 
         (app (app w w) (app u u)))
   
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
Section 2: Finding a substitution, if it exists.

For each pair of ACL2 terms, give a substitution that instantiates the 
first to the second. If no substitution exists write "None" and give a brief
explanation. Notice, terms like (app x) don't have to make sense or be 
syntatically correct for the substitution to be allowed.....but you should still
make sure it's syntatically correct for the rest of the course. :)

Example: (app l m)
         (app (cons 3 nil) nil)
        *((l (cons 3 nil))(m nil))
Again the * is just used to indicate the solution line.

a. (app (list a) (rev (app b a)))
   (app (list (app b (list (first a) b)))(rev (app (rev a)(app b (list (first a) b)))))
   ........
   ((a (app b (list (first a) b))) 
    (b (rev a)))

b. (and (< (* z w) (+ x (+ x 2))) (> z x))
   (and (< (* (unary-/ (+ (- 5 6) 7)) x) (+ (+ x x) (+ (+ x x) 2))) (> (unary-/ (+ (- 5 6) 7)) (+ x x)))
   ........
   ((z (unary-/ (+ (- 5 6) 7)))
    (w x)
    (x (+ x x)))

c. (app y z)
   (app z y)
   .......
   ((y z) 
    (z y))
   
d. (app '(a a) (app b '(1 2 3)))
   (app '(x x) (app y '(1 2 3)))
   ........
   None, the expression '(a a) is a list of symbols 'a, and therefore cannot be 
   replaced using substitution.

e. (in a (app b a))
   (in b (list (app a b) b)) 
   ........
   None, there is no way to change an app expression to a list expression, because 
   ACL2s parses function calls as symbols.

g. (app a (app (cons b c) b))
   (app '(1 2) (app (cons (cons b c) d) (cons b c)))
   ........
   ((a '(1 2))
    (b (cons b c))
    (c d))
   
f. (app (list a b) (app a c))
   (app (list (rev (app a b)) d) (app (rev b) a))
   ........
   None, because a has to be (rev (app a b)) in the first half, but it also has to be 
   (rev b) in the later half. Since we cannot subsititute an expression for two 
   different expressions, there is no way to substitute this.

|#

#|=================================== 
Useful function definitions used later
=====================================

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

 ; Function del is defined the exact same way in case
 ; we sometimes write del instead
(defunc delete (e l)
  :input-contract (listp l)
  :output-contract (listp (delete e l))
  (if (endp l)
    nil
    (if (equal e (first l))
      (rest l)
      (cons (first l)(delete e (rest l))))))

(defunc in (a l)
  :input-contract (listp l)
  :output-contract (booleanp (in a l))
  (if (endp l)
    nil
    (or (equal a (first l)) (in a (rest l)))))

(defunc rev (x)
  :input-contract (listp x)
  :output-contract (listp (rev x))
  (if (endp x)
    x
    (app (rev (rest x))(list (first x)))))
     

(defdata lor (listof rational))

(defunc min-l (lr)
  :input-contract (and (lorp lr)(consp lr))
  :output-contract (rationalp (min-l lr))
  (if (endp (rest lr))
    (first lr)
    (if (< (first lr) (min-l (rest lr)))
        (first lr)
        (min-l (rest lr)))))
        
;; duplicate: List -> List
;; repeats each element in the given list once
(defunc duplicate (l)
  :input-contract (listp l)
  :output-contract (listp (duplicate l))
  (if (endp l)
    nil
    (cons (first l) (cons (first l) (duplicate (rest l))))))
 

NOTE: I'm removing the let in the definition of min-l to 
make using the body more obvious but a let would be equivalent.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; SECTION 3: Instantiating Theorems
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
For each definitional axiom or provided theorem, write the theorem then write 
the substitution used to instantiate the theorem. Recall the substitution is symbol 
manipulation while instantiation is a proof technique showing that replacing 
variables in a theorem results in a theorem. You can assume the theorems provided
are actually theorems.

EX: For a call to (app nil l) substitute nil and l in the definitional axiom of app.
    (implies (and (listp x)(listp y))
             (equal (app x y)(if (endp x) y (cons (first x)(app (rest x) y)))))
    = {Def of app|((x nil)(y l))}
    (implies (and (listp nil)(listp l))
             (equal (app nil l) (if (endp nil) l (cons (first nil)(app (rest nil) l)))))
    
    Notice the above definitional axiom for app has the input contract for app as its 
    left hand side. This needs to be included in an instantiation.  Think about why that is.  

3a. Theorem phi_rev-rev is (implies (listp x)(equal (rev (rev x)) x))
    Instantiate phi_rev-rev substituting x for (list x 2).
    ........
    (implies (listp x) (equal (rev (rev x)) x))
  = {Def of phi_rev-rev | ((x (list x 2)))}
    (implies (listp (list x 2)) 
             (equal (rev (rev (list x 2))) (list x 2)))

3b. Theorem phi_min-newmin is: 
    (implies (and (lorp lr)(consp lr)(rationalp r)(< r (min-l lr)))
             (equal (min-l (cons r lr)) r))
    Instantiate phi_min-newmin with lr as '(3 2 1) and r as 2
    Is this still a theorem?  Why? (this is not a retorical question)
    
    ........
    (implies (and (lorp lr)(consp lr) (rationalp r) (< r (min-l lr)))
             (equal (min-l (cons r lr)) r))
  = {Def of phi_min-newmin | ((lr '(3 2 1)) (r 2))}
    (implies (and (lorp '(3 2 1) (consp '(3 2 1)) (rationalp 2) (< 2 (min-l '(3 2 1)))) 
             (equal (min-l (cons 2 '(3 2 1))) 2)))
    
    This is still a theorem, because the LHS is false, since 2 is not < min-l of '(3 2 1), therefore, anything that comes out of the RHS can be ignored. The theorem is true.
    
3c. 
Instantiate phi_min-newmin (from 3b) with lr as '(3 2 1) and r as -1.
    
    ........
    (implies (and (lorp lr)(consp lr) (rationalp r) (< r (min-l lr)))
             (equal (min-l (cons r lr)) r))
  = {Def of phi_min-newmin | ((lr '(3 2 1)) (r -1))}
    (implies (and (lorp '(3 2 1) (consp '(3 2 1)) (rationalp -1) (< -1 (min-l '(3 2 1)))) 
             (equal (min-l (cons -1 '(3 2 1))) -1)))
    

3d. Instantiate the definitional axiom of delete with the element 'x being deleted from
    list '(x x 1 2): 
    
    ........
    (implies (listp l)
             (equal (delete e l) (if (endp l) nil (if (equal e (first l)) (rest l) (cons (first l) (delete e (rest l)))))))
  = {Def of delete | ((e 'x) (l '(x x 1 2)))}
    (implies (listp '(x x 1 2))
             (equal (delete 'x '(x x 1 2)) (if (endp '(x x 1 2)) nil (if (equal e (first '(x x 1 2))) (rest '(x x 1 2)) (cons (first '(x x 1 2)) (delete 'x (rest '(x x 1 2))))))))

|#


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; SECTION 4: INTRODUCTION TO EQUATIONAL REASONING
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|

Questions in section 4 ask for equational proofs about ACL2s

The definitional axioms (input contract => (function call = function body))
and contract theorems (input contract => output contract) for defined 
functions can be used in the proof. 

You can use ACL2s to check the conjectures you come up with, but you are 
not required to do so. 

Here are some notes about equational proofs although additional information
can be found in the course notes 
(http://www.ccs.neu.edu/course/cs2800f18/rapeq.pdf). Remember the key
consideration when grading your proofs will be how well you communicate 
your ideas and justifications:

1. The context. Remember to use propositional logic to rewrite
the context so that it has as many hypotheses as possible.  See
the lecture notes for details. Label the facts in your
context with C1, C2, ... as in the lecture notes.

2. The derived context: Draw a dashed line (----) after the context 
and add anything interesting that can be derived from the context.  
Use the same labeling scheme as was used in the context. Each derived
fact needs a justification. Again, look at the lecture notes for
more information.

3. Use the proof format shown in class and in the lecture notes,
which requires that you justify each step.

4. THIS ASSIGNMENT ONLY: When using an axiom, theorem or lemma, 
show the name of the axiom, theorem or lemma and then 
*show the substitution* you are using for any definitional axiom, 
or theorem IF a substitution  actually occurs  
(no need to write (n n) for example).  

Ex. Using the definitional axiom for app to conclude (app nil l) = l
you would write 
{Def. app|((x nil)(y l)), 
   if axioms|((x (endp nil)) (y l)(z (cons (first nil)(app (rest nil) l))))}
   
This will be taken with a grain of salt since we only do this in this
assignment, HOWEVER, you should definitely say the substitutions for non-trivial
replacements (it's really to give you the necessary practice). The if axiom
substitution above was probably overkill)

5. When using the definitional axiom of a function, the
justification should say "Def. <function-name>".  When using the
contract theorem of a function, the justification should say
"Contract <function-name>".

6. Arithmetic facts such as commutativity of addition can be
used. The name for such facts is "arithmetic".

7. You can refer to the axioms for cons, and consp as the "cons axioms", 
Axioms for first and rest can be referred to as "first-rest axioms".
The axioms for if are named "if axioms"

8. Any propositional reasoning used can be justified by "propositional
reasoning", "Prop logic", or "PL" except you should use "MP" 
to explicitly identify when you use modus ponens.

8b. Hocus Pocus or "HP" is not a valid justification though many try.

9. For this homework, you can only use theorems we explicitly
tell you you can use or we have covered in class / lab. 
You can, of course, use the definitional axiom and contract 
theorem for any function used or defined in this homework. You
may also use theorems you've proven earlier in the homework.
The definitions used for the remainder of the questions are listed
above.

10. For any given propositional expression, feel free to re-write it
in infix notation (ex A =>(B/\C)). You do not need to justify this.

11. To make your life easier, you can combine the use of if axioms, justifications
for taking a branch, and definitional axioms in one step.
     (app nil l) 
   = {Def. app|((x nil)(y l)), if axioms, Def. of endp|((x nil))}
     l
  This should make for far less typing.  The key point: IT SHOULD BE OBVIOUS WHAT
  YOU ARE DOING.  Combining too many steps makes it hard to follow your work and your
  grade will suffer as a result. Clear or "obvious" steps can be combined.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


For each of the conjectures in section 4:

- perform conjecture contract checking, and add hypotheses if necessary. 
  Contract completion is adding the minimal set of hypotheses to guarantee 
  input contracts for all functions used in the conjecture are satisfied 
  when the hypotheses are satisfied. Do not add anything else to the 
  conjecture.

- run some tests to make an educated guess as to whether the conjecture is
  true or false. 
    - *** Not all conjectures are valid******
       - For falsifiable expressions, give a counterexample and show that 
         it evaluates to false (thus the expression is falsifiable). 
    - For theorems, give a proof using equational reasoning, following 
    instructions 1 to 11 above. 
    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
4a. Phi_rev_del: (rev (del e (rev x))) = (del e x)
You can assume theorem phi_rev_rev: (implies (listp x) (equal x (rev (rev x))))
..................
C1. (listp x) {Contract of rev}

'(1 2 3 1 1 4)  <----- x
'(4 1 1 3 2 1)  <----- (rev x)
'(4 1 3 2 1)    <----- (del 1 (rev x))
'(1 2 3 1 4)    <----- (rev (del 1 (rev x)))

'(2 3 1 1 4)    <----- (del 1 x)

The conjecture is falsifiable, and therefore not valid.

4b. Phi_dupe_first_min: (implies (endp (rest l)))
                                 (equal (min-l l)(min-l (duplicate l))))

.............
C1. (endp (rest l)) 
C2. (lorp l) {Contract of min-l}
C3. (consp l) {Contract of min-l}
-------------
C3. ~(endp l) {consp axiom, C3}

RHS:(min-l (duplicate l))
  = {Def of duplicate}
    (min-l (if (endp l) nil (cons (first l) (cons (first l) (duplicate (rest l)))))))
  = {if axiom, C3}
    (min-l (cons (first l) (cons (first l) (duplicate (rest l)))))
  = {Def of duplicate, if axiom, C1}
    (min-l (cons (first l) (cons (first l) nil)))
  = {Def of min-l | ((lr (cons (first l) (cons (first l) nil))))}
    (if (endp (rest (cons (first l) (cons (first l) nil))))
    	(first (cons (first l) (cons (first l) nil)))
    	(if (< (first (cons (first l) (cons (first l) nil))) (min-l (rest (cons (first l) (cons (first l) nil)))))
        	(first (cons (first l) (cons (first l) nil)))
        	(min-l (rest (cons (first l) (cons (first l) nil)))))))
  = {first-rest axiom | ((l (cons (first l) (cons (first l) nil)))), 
     endp axiom | ((l (cons (first l) nil)))
     if axiom | ((a nil))}
    (if (< (first (cons (first l) (cons (first l) nil))) (min-l (rest (cons (first l) (cons (first l) nil)))))
		(first (cons (first l) (cons (first l) nil)))
        (min-l (rest (cons (first l) (cons (first l) nil))))))
  = {first-rest axiom | ((l (cons (first l) (cons (first l) nil))))}
    (if (< (first l) (min-l (cons (first l) nil)))
        (first (cons (first l) (cons (first l) nil)))
        (min-l (rest (cons (first l) (cons (first l) nil))))
  = {Def of min-l | ((lr (cons (first l) nil)))}
    (if (< (first l) (if (endp (rest (cons (first l) nil))) (first (cons (first l) nil)) (if (< (first (cons (first l) nil)) (min-l (rest (cons (first l) nil))))(first (cons (first l) nil))(min-l (rest (cons (first l) nil))))))
        (first (cons (first l) (cons (first l) nil)))
        (min-l (rest (cons (first l) (cons (first l) nil))))
  = {first-rest axiom | ((l (cons (first l) nil)))
     endp axiom | ((l nil))
     if axiom | ((a t))}
    (if (< (first l) (first (cons (first l) nil)))
        (first (cons (first l) (cons (first l) nil)))
        (min-l (rest (cons (first l) (cons (first l) nil))))
  = {first-rest axiom | ((l (cons (first l) nil)))}
    (if (< (first l) (first l))
        (first (cons (first l) (cons (first l) nil)))
        (min-l (rest (cons (first l) (cons (first l) nil))))
  = {< axiom | ((a (first l)) (b (first l)) 
     if axiom | ((a nil))}
    (min-l (rest (cons (first l) (cons (first l) nil))))
  = {first-rest axiom | ((l (cons (first l) (cons (first l) nil))))}
    (min-l (cons (first l) nil))
  = {Def of min-l | ((lr (cons (first l) nil)))}
    (if (endp (rest (cons (first l) nil)))
   		(first (cons (first l) nil))
    	(if (< (first (cons (first l) nil)) (min-l (rest (cons (first l) nil))))
        	(first (cons (first l) nil))
        	(min-l (rest (cons (first l) nil))))))
  = {first-rest axiom | ((l (cons (first l) nil)))
     endp axiom | ((l nil))
     if axiom | ((a t))}
    (first (cons (first l) nil))
  = {first-rest axiom | ((l (cons (first l) nil)))}
    (first l)

LHS:(min-l l)
  = {Def of min-l | ((lr l))}
    (if (endp (rest l))
   		(first l)
    	(if (< (first l) (min-l (rest l)))
        	(first l)
        	(min-l (rest l)))))
  = {C1, if axiom | ((a t))}
    (first l)

LHS = RHS, therefore the conjecture is valid.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

4c. Phi_lendel: (+ (len (del e l)) 1) = (len l)
C1. (listp l) {Contract of del}
-----------

'(1 2 3) <-- l
'(1 2 3) <-- (del 4 l) 
3        <-- (len (del e l))
4        <-- (+ (len (del e l)) 1)

3        <-- (len l) 

4 != 3, so the conjecture is not valid.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

4d. Phi_len_app1: (len (app (cons e nil) y)) = (+ 1 (len y))
..........
C1. (listp y) {Contract of app / len}
------------------

LHS:(len (app (cons e nil) y))
  = {Def of app | ((x (cons e nil)))}
    (len (if (endp (cons e nil)) 
             y 
             (cons (first (cons e nil)) (app (rest (cons e nil)) y))))
  = {endp axiom | ((l (cons e nil)))
     if axiom | ((a nil))}
    (len (cons (first (cons e nil)) (app (rest (cons e nil)) y)))
  = {first-rest axiom | ((l (cons e nil)))}
    (len (cons e (app nil y)))
  = {Def of app | ((x nil))}
    (len (cons e (if (endp nil) y (cons (first nil) (app (rest nil) y)))))
  = {endp axiom | ((l nil))
     if axiom | ((a t))}
    (len (cons e y))
  = {Def of len | ((x (cons e y)))}
    (if (endp (cons e y)) 0 (+ 1 (len (rest (cons e y)))))
  = {endp axiom | ((l (cons e y)))
     if axiom | ((a nil))}
    (+ 1 (len (rest (cons e y))))
  = {first-rest axiom | ((l (cons e y)))}
    (+ 1 (len y))
  = {QED}

LHS = RHS, so the conjecture is valid



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
4e. Prove phi_min-newmin you wrote above.
.............
(implies (and (lorp lr)(consp lr)(rationalp r)(< r (min-l lr)))
             (equal (min-l (cons r lr)) r))

C1. (lorp lr)
C2. (consp lr)
C3. (rationalp r)
C4. (< r (min-l lr))
--------------

LHS:(min-l (cons r lr))
  = {Def of min-l | ((lr (cons r lr)))}
    (if (endp (rest (cons r lr)))
    	(first (cons r lr))
    	(if (< (first (cons r lr)) (min-l (rest (cons r lr))))
        	(first (cons r lr))
        	(min-l (rest (cons r lr)))))
  = {first-rest axiom | ((l (cons r lr)))}
    (if (endp lr)
        (first (cons r lr))
    	(if (< (first (cons r lr)) (min-l (rest (cons r lr))))
        	(first (cons r lr))
        	(min-l (rest (cons r lr)))))
Case1: (endp lr) is nil
  = {if axiom | ((a nil))}
    (if (< (first (cons r lr)) (min-l (rest (cons r lr))))
        	(first (cons r lr))
        	(min-l (rest (cons r lr))))
  = {first-rest axiom | ((l (cons r lr)))}
    (if (< r (min-l lr))
        (first (cons r lr))
        (min-l (rest (cons r lr))))
  = {C4, if axiom | ((a t))}
    (first (cons r lr))
  = {first-rest axiom | ((l (cons r lr)))}
    r
  = {QED}
Case2: (endp lr) is t
  = {if axiom | ((a t))}
    (first (cons r lr))
  = {first-rest axiom | ((l (cons r lr)))}
    r
  = {QED}

LHS = RHS, so the conjecture is valid.


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
4f. (implies (equal (in a (rest l))
                    (in a (duplicate (rest l))))
             (equal (in a l)
                    (in a (duplicate l))))
.......
C1. (in a (rest l)) = (in a (duplicate (rest l)))
C2. (listp l) {Contract of duplicate}
C3. (listp (rest l)) {Contract of duplicate}
----------
C4. ~(endp l) {Contract of rest, C3}

Show:(in a l) = (in a (duplicate l))

RHS:(in a (duplicate l))
  = {Def of duplicate}
    (in a (if (endp l)
 		  nil
    	  (cons (first l) (cons (first l) (duplicate (rest l))))))
  = {C4, if axiom | ((a nil))}
    (in a (cons (first l) (cons (first l) (duplicate (rest l)))))
  = {Def of in | ((l (cons (first l) (cons (first l) (duplicate (rest l))))))}
    (if (endp (cons (first l) (cons (first l) (duplicate (rest l)))))
		nil
    	(or (equal a (first (cons (first l) (duplicate (rest l)))))) 
			(in a (rest (cons (first l) (duplicate (rest l))))))))
  = {endp axiom | ((l (cons (first l) (cons (first l) (duplicate (rest l))))))
     if axiom | ((a nil))}
    (or (equal a (first (cons (first l) (cons (first l) (duplicate (rest l))))))) 
		(in a (rest (cons (first l) (cons (first l) (duplicate (rest l)))))))
  = {first-rest axiom | ((l (cons (first l) (duplicate (rest l)))))}
    (or (equal a (first l))
        (in a (cons (first l) (duplicate (rest l)))))
  = {Def of in | ((l (cons (first l) (duplicate (rest l)))))}
    (or (equal a (first l))
        (if (endp (cons (first l) (duplicate (rest l))))
		    nil
    		(or (equal a (first (cons (first l) (duplicate (rest l))))) (in a (rest (cons (first l) (duplicate (rest l))))))))
  = {endp axiom | ((l (cons (first l) (duplicate (rest l)))))
     if axiom | ((a nil))}
    (or (equal a (first l))
        (or (equal a (first (cons (first l) (duplicate (rest l))))) 
			(in a (rest (cons (first l) (duplicate (rest l))))))) 
  = {first-rest axiom | ((l (cons (first l) (duplicate (rest l)))))}
	(or (equal a (first l))
        (or (equal a (first l)) 
 			(in a (duplicate (rest l))))) 
  = {PL}
    (or (equal a (first l))
        (in a (duplicate (rest l))))


LHS:(in a l)
  = {Def of in}
    (if (endp l)
  	    nil
        (or (equal a (first l)) (in a (rest l)))))
  = {C4, if axiom | ((a nil))}
    (or (equal a (first l))
        (in a (rest l)))
  = {C1} 
    (or (equal a (first l))
        (in a (duplicate (rest l))))

LHS = RHS, so the conjecture is valid.

|#



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Feedback (10 points)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
#|

We want to gather feedback about how the course is going
so we can improve your learning experience. After all, if
we only get feedback in the TRACE evaluations this won't help
you; just subsequent classes.

Please fill out the following form.

https://goo.gl/forms/fXHoEdqjzo0M8lL03

We do not keep track of who submitted what, so please be honest. Each
individual student should fill out the form, e.g., if there are two
people on a team, then each of these people should fill out the form.
Only fill out the provided survey once since we can't identify multiple 
submissions from the same person and multiple responses skew the data.

After you fill out the form, write your name below in this file, not
on the questionnaire. We have no way of checking if you submitted the
file, but by writing your name below you are claiming that you did,
and we'll take your word for it.  

10 points will be awarded to each person who fills out the survey. 
If one member doesn't fill out the survey, indicate this. We'll 
give everyone else the points.

The following team members filled out the feedback survey provided in 
the link above:
---------------------------------------------
Kevin Zhang
Jemin Park

|#
```




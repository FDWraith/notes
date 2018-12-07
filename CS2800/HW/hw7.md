

```lisp
#|

CS 2800 Homework 7 - Fall 2018

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

This homework is to be done in a group of 2-3 students. It is designed
to give you practice with function admissibility and introduce you to
measure functions.

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

For this homework you will NOT need to use ACL2s. However, you could
use the Eclipse/ACL2s text editor to write up your solution.

|#


#|

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Admissible or not?
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

For each of the definitions below, check whether it is theoretically 
admissible, i.e. it satisfies all rules of the definitional principle. 
You can assume that Rule 1 is met: the symbol used in the defunc is a new 
function symbol in each case.

If you claim admissibility, BRIEFLY

1. Explain in English why the body contracts hold.
2. Explain in English why the contract theorem holds.
3. Suggest a measure function that can be used to show termination.
   (You DO NOT have to prove the measure function properties in this problem.)

Otherwise, identify a rule in the Definitional Principle that is violated.

If you blame one of the purely syntactic rules (variable names,
non-wellformed body etc), explain the violation in English.

If you blame one of the semantic rules (body contract, contract theorem or
termination), you must provide an input that satisfies the input contract, but
causes a violation in the body or violates the output contract or causes
non-termination.

Remember that the rules are not independent: if you claim the function does
not terminate, you must provide an input on which the function runs forever
*without* causing a body contract violation: a body contract violation is
not a counterexample to termination. Similarly, if you claim the function
fails the contract theorem, you must provide an input on which it
terminates and produces a value, which then violates the output contract.

Your explanations should be brief but clear. We are not looking for a page 
of text per question but we also want to clearly see that you understand 
the function and if/what is wrong with it.

I used the term "theoretically admissible" because for some functions below
you can demonstrate they are admissible but ACL2s won't actually admit it 
without a lot of extra guidance from you (this isn't your responsibility).

Admissibility
1. Name is okay
2. Body is well formed
3. Variable symbols are distinct
4. function terminates
5. IC => OC
6. body contracts hold under assumption of IC

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; SECTION 1: Admissibility
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
1.

(defunc f1 (n)
  :input-contract (natp n)
  :output-contract (rationalp (f1 n))
  (if (equal n 0)
    n
    (f1 (/ n 2))))

............
The function is not admissible, because if n is odd, (/ n 2) does not produce a nat, so the recursive call cannot happen (body contracts don't hold). 
Ex. (f1 3)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
2.

(defunc f2 (a b)
  :input-contract  (and (listp a) (listp b))
  :output-contract (posp (f2 a b))
  (cond ((or (endp a) (endp b)) 1)
        ((> (len a) (len b)) 
         (+ 1 (f2 (rest a) (cons (first a) b))))
        ((equal (len a) (len b)) 1)
        (t                
         (+ 1 (f2 (cons (first b) a)(rest b))))))

............
The function is not admissible. 
Ex. a = (list 1 2)
    b = (list 3)
The first of the longer list (a) would be passed back and forth in the recursive calls, so the function does not terminate.


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
3.

(defunc f3 (x y)
  :input-contract (and (listp x)(listp y))
  :output-contract (listp (f3 x y))
  (cond ((< (len x) (len y)) (f3 (cons (first y) x) y))
        ((> (len x) (len y)) (f3 (rest x) y))
        (t   x)))

............
The function is admissible.
In all three cond cases, the output is a list, asssuming the output contract for f3 holds. 
The body contracts hold. In the first cond case, (first y) is okay, because (len y) > (len x). (first y) would fail if (endp y), but then that would mean (len y) = 0, which can't be greater than (len x). The same is true for (rest x) in the second cond case. Thus, the recursive calls to f3 are okay.
The function terminates, and a measure function would be the absolute value of (len x) - (len y). 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
4.

(defunc f4 (x y)
  :input-contract (and (natp x) (posp y))
  :output-contract (posp (f4 x y))
  (cond ((equal y 1) y)
        ((equal x 0) x)
        ((< y x)     (f4 y x))
        (t           (f4 x (- y 1)))))

............
The function is not admissible. 
The output contract for the function doesn't hold for the second cond case, because x would not satisfy posp if x = 0.
Ex. (f4 0 20)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
5. (challenging....probably not for points)

(defunc f5 (y z)
  :input-contract (and (rationalp y) (rationalp z))
  :output-contract (rationalp (f5 y z))
  (cond ((<= y  0) z)
        ((> z 0) (f5 y (- z y)))
        (t z)))

z = 25, y = 5, m = 5
z = 20, y = 5, m = 4
...
z = 0, y = 5, m = 0

Think about if any variables converge towards a value
and how much they are guaranteed to converge each recursive call.
Can you convert this amount to a natural?  FYI: (acl2::ceiling x y)
exists as a function if that helps. It's the ceiling of x/y.

............
The function is admissible.
The output contract is satisfied in each of the three cond cases, because z is a rational, and f5 must produce a rational. 
The body contract is satisfied for f5, since y is a rational (from IC), and (- z y) is also a rational, sicne there are no bounds for rational numbers.
The function terminates with a measure function of (if (> y 0) (acl2::ceiling z y) 0)


    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
6.

(defunc f6 (x y)
  :input-contract (and (listp x)(natp y))
  :output-contract (natp (f6 x y))
  (cond ((equal y 0) (len x))
        ((endp x)    0)
        (t           (f6 (len x) (list y)))))

............
The function is not admissible, because the body contract of (f6 (len x) (list y)) is not satisfied. f6 takes a list, followed by a number, not vice-versa.
Ex. (f6 (list 1 2) 3)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
7.

(defunc f7 (i)
  :input-contract (integerp i)
  :output-contract (integerp (f7 i))
  (if (< i -5)
    i
    (- f7 (* i i))))

............
The function is not admissible because it is not well-formed
(- f7 (* i i)) doesn't make any sense, since f7 is the function name, not a variable name.
Ex. (f7 20)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
8.

(defunc f8 (x y)
  :input-contract (and (posp x) (integerp y))
  :output-contract (posp (f8 x y))
  (if (>= y x)
    (+ x y)
    (+ (* y y) (f8 (+ x 1) (+ y 2)))))

m(1 0) > m(1 2)
   1        0
m(1 -2) > m(1 0) > m(1 2)
   2        1         0
m(1 -1) > m(1 1)
   1        0
x - (ceil y / 2)

............
The function is admissible.
The output contract is satisfied, because when y >= x, which makes y a positive, the function produces (+ x y), which must be a positive. When y < x, the function produces (+ (* y y) (f8 (+ x 1) (+ y 2))) where (* y y) is the square of y (always pos), and f8 produces pos.
The body contract is satisfied for recursive call of f8, because (+ x 1) is pos, and (+ y 2) is another integer.
The function terminates with measure function of (- x (acl2:ceiling y 2))

    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
9.

(defunc f9 (i x j)
  :input-contract (and (posp i) (listp x) (posp i))
  :output-contract (posp (f9 i x j))
  (cond ((equal j 1) (+ (len x) j))
        ((equal i 1) (+ i j))
        (t           (f9 (- i 1) (cons (+ i j) x) (- j 1)))))
    
............
The function is not admissible, because j can be anything (given by thei input contract), so the body contracts will fail (like (+ (len x) j)). 
Ex. (f9 10 (list 1 2) "hello")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
10.

(defunc f10 (a b)
  :input-contract  (and (natp a) (posp b))
  :output-contract (posp (f10 a b))
  (cond ((or (equal a 0)(equal b 1)) 1)
        ((> a b) (f10 b a))
        (t (+ 1 (f10 a (- b 1))))))

m(0, 1) 
m(1, 1)
m(2, 1)

m(20, 19) > m(19, 20) > m(19, 19) > m(19, 18) > m(18, 19) > ...
    60         59          58           57
m(20, 5) > m(5, 20) > m(5, 19) > m(5, 18) > ... > m(5, 5) > m(5, 4) > m(4, 5) > m(4, 4) > m(4, 3) > m(3, 4) > m(3, 3) > m(3, 2) > m(2, 3) > m(2, 2) > m(2, 1)
         6         5         4         3         2         1          0

m(5, 2) > m(2, 5) > m(2, 4) > m(2, 3) > m(2, 2) > m(2, 1)
   5         4         3          2         1         0
   3         3         2          1         0         1
m(5, 3) > m(3, 5) > m(3, 4) > m(3, 3) > m(3, 2) > m(2, 3) > m(2, 2) > m(2, 1)
   7         6         5          4         3         2        1         0
   15        15        12         9         6         6        4         2
   16                                       7

if a > b:
  ab + 1 > ab


............
The function is admissible.
The output contract is satisfied in all three cond cases, because 1 is pos, and f10 must produce a pos, and (+ 1 pos) is a pos.
The body contract is satisfied, because swapping b and a is fine, since we know that a > b, so we are swapping two positive numbers. The second recursive call is fine as well, because (- b 1) can only violate IC of f10 if it is is 0, but because we know that b > 1, this is not possible.
The function terminates with a measure function of 
(if (equal b 1)
    0
    (if (> a b))
        (+ 1 (* a b))
        (* a b)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
11.

(defunc f11 (x n)
  :input-contract (and (listp x) (integerp n))
  :output-contract (loip (f11 x n))
  (if (endp x)
    nil
    (if (< n 0) 
      (cons (first x) (f11 (rest x) (+ n 1)))
      (f11 (cons n (rest x)) (- n 1)))))

............
The function is not admissible.
The output contract won't hold, because x is list of anything, so (cons (first x) ...) doesn't produce a list of integers. 
Ex. (f11 (list 'a 'b) 1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
12.

; if you want to push ACL2s through these functions, put it in program mode here.
;:program
(defunc f12 (p i)
  :input-contract (and (posp p)(integerp i))
  :output-contract (posp (f12 p i))
  (cond ((equal p 1)   10)
        ((< p 9)       (- (f12 (- p 1) i) 1))
        ((> i p)       (f12 i (- p 2)))
        (t             (f12 (- p 1) i))))

      
 ............
m(20 18) > m(19 18) > m(18 18) > m(17 18) > m(18 15) > ... > m(9 i) ...


The function is admissible.
The output contract is satisifed for each of the cond cases.
For the first case, 10 is a positive, so that is fine. For the second case, p < 9, so the recursive call is to another p < 8. this continues until the first case, in which case the call returns 10. 10 - 1 - 1 ... up to 8 -1's is still postive. For the third case, the arguments are switched, so it will result in one of the other cases. The fourth case continuously decreases p until it matches either the first, second, or third case upon which it returns a positive.
The body contract is satisified as well. In case 2, we know that 1 < p < 9, and i is unchanged. In case 3, i > p, so i > pos, therefore (posp i). Finally, the third case is when p >= 9, so (- p 1) is pos. 
The function terminates with a measure function of (if (p >= i) (* p i) p)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
13.

(defunc f13 (p r)
  :input-contract (and (posp p)(rationalp r))
  :output-contract (posp (f13 p r))
  (cond ((equal p r) 10)
        ((< p r)  (+ (f13 (+ p (numerator r)) r) 1))
        (t  (f13 p (+ r 1)))
        
............
The function is not admissible, because it does not terminate.
Ex. (f13 5 16/15)

        
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
14.

(defunc f14 (x y)
  :input-contract (and (posp x) (posp y))
  :output-contract (integerp (f14 x y))
  (cond ((equal x y) 5)
        ((< x y)     (f14 y x))
        ((< x 1)     (f14 x y))
        (t           (- (f14 (- x 1) y) 3))))

............
Ex. (f14 1 1)
The function is not admissible, because the body contract for the last cond case will fail since (- x 1) = 0, which is not positive.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

15.

(defunc f15 (x y)
  :input-contract  (and (natp x) (natp y))
  :output-contract (posp (f15 x y))
  (cond ((equal x y)    1)
        ((> x y)        (f15 y x))
        (t              (f15 x (- y 2)))))


............
Ex. (f15 0 1)
The function is not admissible, because the body contract in the last cond cause will fail since (- y 2) = -1, which is not a nat.


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

16.

(defunc f16 (x y)
  :input-contract (and (listp x) (posp y))
  :output-contract (listp (f16 x y))
  (cond ((endp x)     (list y))
        ((equal y 1)  (f16 x (len x)))
        (t            (f16 (rest x) (- y 1)))))
  
............
Ex. (f16 (list 2) 1)
The function is not admissible, because it does not terminate. If (len x) is 1, then it will constantly produce (f16 x 1).
        
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

17.

(defunc  f17 (l)
  :input-contract (listp l)
  :output-contract (booleanp (f17 l))
  (cond ((endp l)  l)
        ((in e l)  t)
        (t         (cons (first l)(f17 (rest l))))))

............
Ex. (f17 (list 1))
The function is not admissible, because what is e? (the body is not well-formed)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

|#
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; SECTION 2: DOES IT TERMINATE?
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; For each of the following functions, mention whether the function terminates 
;; or not. If it does, give a measure function for it AND write the proof obligations
;; needed to prove termination (here we are not asking  you to prove anything). 
;; Features of a valid measure function are described
;; in section 3 below and in the notes.
;; If it does not terminate, give a concrete input on which it fails.
;; Here is a function you can use to help you

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; GIVEN
;; abs: integer -> natural
;; (abs i) takes an integer value
;; and returns the absolute value
;; of that number (thus a natural number)
(defunc abs (i)
  :input-contract (integerp i)
  :output-contract (natp (abs i))
  (if (< i 0) (unary-- i)  i))
#|
;; EXAMPLE: for the function app the solution would be
(defunc m-app (x y)
  :input-contract (and (listp x)(listp y))
  :output-contract (listp (m-app x y))
  (len x))

;Proof Obligation
(listp x) /\ (listp y) /\ ~(endp x) 
   => (m-app x y) = (len x) > (len (rest x)) = (m-app (rest x) y)
|#

#|
(defunc f18 (r)
  :input-contract (rationalp r)
  :output-contract (integerp (f18 r))
  (if (equal r 0)
    14
    (+  (f18 (- (abs (numerator r)) 1)) 4)))

............
The function terminates
(defunc m-f18 (r)
	:input-contract (rationalp r)
    :output-contract (natp (m-f18 r))
    (abs (numerator r)))

proof obligation:
~(r = 0) /\ (rationalp r) => (m-f18 r) > (m-f18 (- (abs (numerator r)) 1))
 


(defunc  f19 (x)
  :input-contract (integerp x)
  :output-contract (integerp (f19 x))
  (cond ((equal x 0) 1)
        ((> x 0) (* x (f19 (- x 1))))
        (t (* x (f19 (+ x 1))))))

............
The function terminates
(defunc m-f19 (x)
	:input-contract (integer x)
    :output-contract (natp (m-f19 x))
    (abs x))

proof obligation:
(integer x) /\ ~(x = 0) /\ (> x 0) => (m-f19 x) > (m-f19 (- x 1))
(integer x) /\ ~(x = 0) /\ ~(> x 0) => (m-f19 x) > (m-f19 (+ x 1))

  
(defunc  f20 (l i)
  :input-contract (and (listp l)(integerp i))
  :output-contract (natp (f20 l i))
  (if (< i 0) 
    (len l)
    (f20 l (- i (len l)))))

............
the function doesn't terminate
Ex. (f20 nil 20)

 
(defunc  f21 (l1 l2)
  :input-contract (and (listp l1)(listp l2))
  :output-contract (booleanp (f21 l1 l2))
  (cond ((endp l1)    t)
        ((endp l2)  (f21 (rest l1) l2))
        (t    (f21 (cons (first l2) l1) (rest l2)))))

............
The function terminates
(defunc m-f21 (l1 l2)
	:input-contract (and (listp l1) (listp l2))
	:output-contract (natp (m-f21 l1 l2))
    (+ (* (len l1) (len l2)) (len l1)))

proof obligation:
(listp l1) /\ (listp l2) /\ (endp l2) /\ ~(endp l1) => (m-f21 l1 l2) > (m-f21 (rest l1) l2)
(listp l1) /\ (listp l2) /\ ~(endp l2) /\ ~(endp l1) => (m-f21 l1 l2) > (m-f21 (cons (first l2) l1) (rest l2))



(defunc f22 (n m)
  :input-contract (and (integerp n)(integerp m))
  :output-contract (integerp (f22 n m))
  (cond ((equal n m)                 1)
        ((< n m)  (f22 (+ n 1)(- m 1)))
        (t             (f22 (- n 1) m))))
............
the function terminates
(defunc (m-f22 (n m)
	:input-contract (and (integerp n) (integerp m))
	:output-contract (natp (m-f22 n m)))
	(if (< n m)
		(* 2 (abs (- n m))) + 1
        (* 2 (abs (- n m)))))
proof obligation:
(integerp n) /\ (integerp m) /\ (< n m) /\ ~(n = m) => (m-f22 n m) > (m-f22 (+ n 1) (- m 1))
(integerp n) /\ (integerp m) /\ ~(< n m) /\ ~(n = m) => (m-f22 n m) > (m-f22 (- n 1) m)


|#  


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Section 3: Proving a Measure Function
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
#|

You can prove a function f terminates using a measure function m. 
This requires the following conditions are met:

Condition 1. m has the same arguments and the same input contract as f.
Condition 2. m's output contract is (natp (m ...))
Condition 3. m is admissible.
Condition 4. On every recursive call of f, given the input contract and 
   the conditions that lead to that call, m applied to the arguments in
   the call is less than m applied to the original inputs.

You should do this proof as shown in class (which is also the way we will
expect you to prove termination on exams):

- Write down the propositional logic formalization of Condition 4.
- Simplify the formula.
- Use equational reasoning to conclude the formula is valid.

Unless clearly stated otherwise, you need to follow these steps for EACH
recursive call separately.

Here is an example.

(defunc f (x y)
  :input-contract (and (listp x) (natp y))
  :output-contract (natp (f x y))
  (if (endp x)
    (if (equal y 0) 
      0
      (+ 1 (f x (- y 1))))
    (+ 1 (f (rest x) y))))

The measure is
(defunc m (x y)
  :input-contract (and (listp x) (natp y))
  :output-contract (natp (m x y))
  (+ (len x) y))


For the first recursive call in f, the propositional logic formalization 
for proving Condition 4 is:
(implies (and (listp x) (natp y) (endp x) (not (equal y 0)))
         (< (m x (- y 1)) (m x y)))

This can be rewritten as:
(implies (and (listp x) (natp y) (endp x) (> y 0))
         (< (m x (- y 1)) (m x y)))
         
Proof of Condition 4 for the first recursive call:
Context
C1. (listp x)
C2. (natp y)
C3. (endp x)
C4. (> y 0)

(m x (- y 1))
= { Def m, C3, Def len, Arithmetic }
(- y 1)
< { Arithmetic }
y
= { Def m, C3, Def. len, Arithmetic }
(m x y)

The propositional logic formalization for Proof of Condition 4 for the 
second recursive call:
(implies (and (listp x) (natp y)(not (endp x)))
         (< (m (rest x) y) (m x y)))

Proof:
C1. (listp x)
C2. (natp y)
C3. (not (endp x))

(m (rest x) y)
= { Def m, C3 }
(+ (len (rest x)) y)
< { Arithmetic, Decreasing len axiom }
(+ (len x) y)
= { Def m }
(m x y)

Hence f terminates, and m is a measure function for it.
QED


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

23. Prove f23 terminates:

(defunc f23 (x y)
  :input-contract (and (natp x) (natp y))
  :output-contract (integerp (f23 x y))
  (cond ((and (equal x 0)(equal y 0)) 0)
        ((equal x 0) (f23 256 (- y 1)))
        (t          (f23 (- x 1) y))))

256y + 1
256y
256(y - 1) + 255 = 256y - 1
.....in case you are curious as to what this is, 
think about a 2D array
        
        
What is the measure function for f23?
(defunc m-f23 (x y)
	:input-contract (and (natp x) (natp y))
	:output-contract (natp (m-f23 x y))
	(+ (* 257 y) x))

What is the maximum number of recursive calls this
function can ever make?
257 * y + x


What are the proof obligations?
(natp x) /\ (natp y) /\ (x = 0) /\ ~(y = 0) => (m-f23 x y) > (m-f23 256 (- y 1))
(natp x) /\ (natp y) /\ ~(x = 0) /\ ~(y = 0) => (m-f23 x y) > (m-f23 (- x 1) y)

     
Now prove that m23 is a measure function for f23 using equational reasoning

Case 1:
C1. (natp x)
C2. (natp y)
C3. (x = 0)
------------------
LHS:(m-f23 x y)
  = {Def of m-f23, C3}
    (+ (* 257 y) 0)
  = {Arithmetic}
    (* 257 y)
RHS:(m-f23 256 (- y 1))
  = {Def of m-f23
    (+ (* 256 (- y 1)) 256)
  = {Arithmetic}
    (+ (- (* 257 y) 257) 256)
  = {Arithmetic}
    (- (* 257 y) 1)
  = {LHS}
    (- LHS 1)
LHS > RHS, so Case 1 is true



...
Case 2:
C1. (natp x)
C2. (natp y)
C3. ~(x = 0)
LHS:(m-f23 x y)
  = {Def of m-f23}
    (+ (* 257 y) x)
RHS:(m-f23 (- x 1) y)
  = {Def of m-f23}
    (+ (* 257 y) (- x 1))
  = {Arithmetic}
    (- (+ (* 257 y) x) 1)
  = {LHS}
	(- LHS 1)
LHS > RHS, so Case 2 is true.
  

..........


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
24. Prove f24 terminates (challenge)
Hint: Take lists like l1='(a b c) and l2 = '(d e) with flag = true
How many recursive calls happen?
|#

(defunc f24 (l1 l2 flag)
  :input-contract (and (listp l1)(listp l2)(booleanp flag))
  :output-contract (listp (f24 l1 l2 flag))
  (cond ((and (endp l1) flag)  (f24 l1 l2 nil))
        (flag (f24 (rest l1) (cons (first l1) l2) flag))
        ((not (endp l2))             (f24 (cons (first l2) l1) (rest l2) flag))
        (t                           l1)))
#|
...............

If flag is t, f24 empties l1 into l2
Else, f24 empties l2 into l1, and returns at end

(defunc m-f24 (l1 l2 flag)
	:input-contract (and (listp l1) (listp l2) (booleanp flag))
	:output-contract (natp (m-f24 l1 l2 flag))
	(if flag
		(+ (+ (* 2 (len l1)) (len l2)) 1)
		(len l2)))

proof obligations
(listp l1) /\ (listp l2) /\ (booleanp flag) /\ (endp l1) /\ flag => (m-f24 l1 l2 flag) > (m-f24 l1 l2 nil)
(listp l1) /\ (listp l2) /\ (booleanp flag) /\ ~((endp l1) /\ flag) /\ flag=> (m-f24 l1 l2 flag) > (m-f24 (rest l1) (cons (first l1) l2) flag)
(listp l1) /\ (listp l2) /\ (booleanp flag) /\ ~((endp l1) /\ flag) /\ ~flag /\ ~(endp l2) => (m-f24 l1 l2 flag) > (m-f24 (cons (first l2) l1) (rest l2) flag)

Case 1:
C1. (listp l1)
C2. (listp l2)
C3. (booleanp flag)
C4. (endp l1)
C5. flag
---------------
C6. (len l1) = 0 {Def of len, C4, if axiom}


LHS:(m-f24 l1 l2 flag)
  = {Def of m-f24, C5, if axiom}
    (+ (+ (* 2 (len l1)) (len l2)) 1)
  = {C6}
    (+ (len l2) 1)
  = {RHS}
    (+ RHS 1)
RHS:(m-f24 l1 l2 nil)
  = {Def of m-f24, if axiom}
    (len l2)
LHS > RHS, so Case 1 is t.

Case 2:
C1. (listp l1)
C2. (listp l2)
C3. (booleanp flag)
C4. ~((endp l1) /\ flag)
C5. flag
--------------
C6. ~(endp l1) {C4, DeMorgans, C5, Identity}
C7. (len (cons (first l1) l2)) = (+ (len l2) 1) {Def of len, first-rest axiom}
C8. (+ 1 (len (rest l1)) = (len l1) {Def of len, C6, first-rest axiom}
C9. (len (rest l1)) = (- (len l1) 1) {Arithmetic, C8}


LHS:(m-f24 l1 l2 flag)
  = {Def of m-f24, C5, if axiom}
    (+ (+ (* 2 (len l1)) (len l2)) 1)
  = {RHS}
    (+ RHS 1)
RHS:(m-f24 (rest l1) (cons (first l1) l2) flag)
  = {Def of m-f24, C5, if axiom}
    (+ (+ (* 2 (len (rest l1))) (len (cons (first l1) l2))) 1)
  = {C7, C9}
	(+ (+ (* 2 (- (len l1) 1)) (+ (len l2) 1)) 1)
  = {Arithmetic}
    (+ (+ (- (* 2 (len l1)) 2) (len l2)) (+ 1 1))
  = {Arithmetic}
    (+ (+ (* 2 (len l1)) (len l2)) (- 2 2))
  = {Arithmetic}
    (+ (* 2 (len l1)) (len l2))
LHS > RHS, so Case 2 is t.

Case 3:
C1. (listp l1)
C2. (listp l2)
C3. (booleanp flag)
C4. ~((endp l1) /\ flag)
C5. ~flag
C6. ~(endp l2)
---------------
C7. (len l2) = (+ (len (rest l2)) 1) {Def of len, C6, if axiom}
C8. (len (rest l2)) = (- (len l2) 1) {C7, Arithmetic}

LHS:(m-f24 l1 l2 flag)
  = {Def of m-f24, C5, if axiom}
    (len l2)
RHS:(m-f24 (cons (first l2) l1) (rest l2) flag)
  = {Def of m-f24, C5, if axiom}
    (len (rest l2))
  = {C8}
    (- (len l2) 1)
  = {LHS}
    (- LHS 1)
LHS > RHS so Case 3 is t.

We have proven all three cases, so the function must terminate.
|#

```


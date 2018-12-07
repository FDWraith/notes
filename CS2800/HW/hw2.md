```lisp
#|

CS 2800 Homework 2 - Fall 2018

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
Names of ALL group members: Kevin Zhang, Jemin Park

There will be a 10 pt penalty if your names do not follow this format.
Names of ALL group members: Kevin Zhang, Jemin Park

* Later in the term if you want to change groups, the person who created
  the group should stay in the group. Other people can leave and create 
  other groups or change memberships (the Axel Rose membership clause). 
  We will post additional instructions about this later.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

For this homework you will need to use ACL2s.

Technical instructions:

- open this file in ACL2s as hw02.lisp

- make sure you are in BEGINNER mode. This is essential! Note that you can
  only change the mode when the session is not running, so set the correct
  mode before starting the session.

- insert your solutions into this file where indicated (usually as "....")

- only add to the file. Do not remove or comment out anything pre-existing unless
  we tell you to.

- make sure the entire file is accepted by ACL2s. In particular, there must
  be no "..." left in the code. If you don't finish all problems, comment
  the unfinished ones out. Comments should also be used for any English
  text that you may add. This file already contains many comments, so you
  can see what the syntax is.

- when done, save your file and submit it as hw02.lisp

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

|#

#|

Since this is our first programming exercise, we will simplify the
interaction with ACL2s somewhat: instead of asking it to formally *prove*
the various conditions for admitting a function, we will just require that
they be *tested* on a reasonable number of inputs. This is achieved using
the following directive (do not remove it!):
|#
:program

#|

Notes:

1. Testing is cheaper but less powerful than proving. So, by turning off
proving and doing only testing, it is possible that the functions we are
defining cause runtime errors even if called on valid inputs. In the future
we will require functions complete with admission proofs, i.e. without the
above directive. For this first homework, the functions are simple enough
that there is a good chance ACL2s's testing will catch any contract or
termination errors you may have.

2. The tests ACL2s runs test only the conditions for admitting the
function. They do not test for "functional correctness", i.e. does the
function do what it is supposed to do? ACL2s has no way of telling what
your function is supposed to do. That is what your own tests are for.

3. For now, testing is written using check= expressions.  These take the following
format (and should be familiar to those of you that took Fundies I)
(check= <expression> <thing it should be equal to>)
EX: (check= (- 4/3 1) 1/3)

|#

#|
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 Part I:
 The functions below should warm you up for the rest of the 
 assignment.
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 But first, let me get ahead of myself.  Typically we would ask
 these sorts of questions for HW03 or 4 but we need a special kind
 of list: a list of rationals (lor).  The data definition below
 creates a data type called a list of rationals which is a list that
 can ONLY have rational numbers in it (no need to test if a list
 element is a rational, just need to check if the list is empty or not).
 This list also has a built in recognizer "lorp" which tests if 
 a variable is a list of rationals. I've also created a list of natural 
 numbers that can be recognized by calling "lonp"
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
|#
;; Lists of rational numbers
(defdata lor (listof rational))
;; Lists of natural numbers
(defdata lon (listof nat))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; DEFINE
;; how-many: All x List -> Nat
;;
;; (how-many e l) returns the number of occurrences of element e 
;; in list l.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defunc how-many (e l)
  :input-contract (listp l)
  :output-contract (natp (how-many e l))
  (if (endp l) 
      0
  	  (if (equal e (first l)) 
          (+ 1 (how-many e (rest l)))
          (how-many e (rest l)))))

(check= (how-many  1 ())     0)
(check= (how-many  1 '(1 1)) 2)
;; Remember to add additional tests
(check= (how-many  1 '(1 (1 1) 1)) 2)
(check= (how-many  1 '(2 2 2 3 11)) 0)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; sum: LOR -> Rational
;; (sum lr) computes the sum of the list of rationals (lr)
(defunc sum (lr)
    :input-contract (lorp lr)
	:output-contract (rationalp (sum lr))
	(if (endp lr)
        0
        (+ (first lr) (sum (rest lr)))))

(check= (sum ()) 0)
(check= (sum '(1 2 3)) 6)
(check= (sum '(5)) 5)
(check= (sum '(7/2 5/2)) 6)

;; DEFINE
;; mean: LOR -> Rational
;; (mean lr) The mean or average of list of rationals (lr) is their 
;; total divided by the number of elements in the list.  Feel free
;; to use a helper function.
;; You can assume the mean of an empty list is 0 (it's not and we should
;; ensure lr is non-empty but we aren't ready for that yet).
(defunc mean (lr)
    :input-contract (lorp lr)
	:output-contract (rationalp (mean lr))
    (if (endp lr)
        0
        (/ (sum lr) (len lr))))

(check= (mean ()) 0)
(check= (mean '(1 1 1 1 1 1 1 1)) 1)
(check= (mean '(1 2 3 3 2 1)) 2)
(check= (mean '(5/2 5/2 5 0)) 5/2)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Sqr: Rational -> Rational
;; (sqr r) calculates the square of a rational number (r)
(defunc sqr (r)
    :input-contract (rationalp r)
    :output-contract (rationalp (sqr r))
  	(* r r))

(check= (sqr 2) 4)
(check= (sqr 5) 25)
(check= (sqr -5) 25)
(check= (sqr 0) 0)
(check= (sqr 5/2) 25/4)

;; Helper for var
;; Diff: LOR Rational -> LOR
;; (diff lr r) computes the result of having each number in a list of rationals (lr) subtract r, and then squared. It then returns the resulting list
(defunc diff (lr r)
    :input-contract (and (rationalp r) (lorp lr))
    :output-contract (lorp (diff lr r))
    (if (endp lr) 
        nil
        (cons (sqr (- (first lr) r)) (diff (rest lr) r))))

(check= (diff () 20) ())
(check= (diff '(1) 1) '(0))
(check= (diff '(1 2 3) 2) '(1 0 1))

;; DEFINE: More challenging
;; Var: LOR -> Rational
;; (var lr) calculates the variance of a list of rationals lr.
;; This standard statistical test shows how much numbers vary from
;; each other. You calculate how much each number in lr differs from 
;; the mean and then square that number. The sum  of all of these squared
;; differences divided by the number of elements gives you the variance
;; (and the standard deviation is the square root of that but we can't
;; calculate irrational numbers).
(defunc var (lr)
	:input-contract (lorp lr)
    :output-contract (rationalp (var lr))
	(if (endp lr)
        0
        (/ (sum (diff lr (mean lr))) (len lr))))

(check= (var '(1 1 3 3)) 1)
(check= (var '(-2 -1 1 2)) 5/2)
(check= (var nil) 0)
(check= (var '(1 1 1 1 1 1 1 1 1 1 1)) 0)
(check= (var '(1 1 1 1 300)) 357604/25)
#|
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 Part II: Crazy List Search
 Typically we use a function like find (below) that traverses a list
 and returns true if e is in the list l.  However, what if e is in a
 sub-list of l? Write a function num-found-r which returns the number
 of occurances of e in list l including if e is in a sublist of l. You can
 assume that everything in l is a list OR an atom (at least you can treat
 the data this way)
 Also Notice: (list 1 2 3) contains (list 2 3) because it is really 
 (cons 1 (cons 2 (cons 3 nil)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
|#

;; DEFINE
;; num-found-r: All x List -> Nat
;; (num-found-r e l) takes any element e (which may be a list)
;; and a list l and determines how many occurances of e are in l
;; (if e = l then return 1)
;; The -r indicates that the search is recursive so if e is in a
;; a sublist of l, it should be found and counted.
(defunc num-found-r (e l)
    :input-contract (listp l)
    :output-contract (natp (num-found-r e l))
    (cond 
        ((endp l) 0)
        ((equal e l) 1)
        ((equal e (first l)) (+ 1 (num-found-r e (rest l))))
        ((listp (first l)) (+ (num-found-r e (first l)) 
                              (num-found-r e (rest l))))
        (t (num-found-r e (rest l)))))
	#|    
    (if (endp l) 
      0
  	  (if (equal e (first l)) 
          (+ 1 (num-found-r e (rest l)))
          (if (listp (first l))
              (+ (num-found-r e (first l)) (num-found-r e (rest l)))
              (num-found-r e (rest l))))))
	|#

(check= (num-found-r '(1 2 3) '(2 (1 2 3) (4 3 (1 2 3)))) 2)
(check= (num-found-r '(1 2 3) '(2 (1 2 3) (4 3 (1 2 3 4)))) 1)
(check= (num-found-r 2 '(2 (1 2 3) (2 2 (1 3 4 5)))) 4)
(check= (num-found-r '(2 3) '(2 (1 2 3) (4 3 (1 2 3 4) 2 3))) 2)
(check= (num-found-r nil '(1 (1 (1 (1 (1)))))) 0)
(check= (num-found-r 5 '()) 0)
(check= (num-found-r '(3 4) '(2 (1 2 3 4 5) (3 4) (2 3 4))) 2)

;; GIVEN
;; Probably defined in class. Removes
;; the first instance of an element in a list
(defunc delete (e l)
  :input-contract (listp l)
  :output-contract (listp (delete e l))
  (if (endp l)
    l
    (if (equal e (first l))
      (rest l)
      (cons (first l)(delete e (rest l))))))

;; Ignore. Uncomment if you wanted to use logic mode
;(sig delete (all (listof :b)) => (listof :b))


;; Rewrite num-found-r (as num-found2-r) such that an element is found
;; no matter if e is a permuation of part of l.
;;..................

;; Perm-Equal: All x All -> Boolean
;; (perm-equal a b) determines if a is equal to b, no matter if a 
;; is a permutation of b (if they are both lists)
(defunc perm-equal (a b)
    :input-contract t
    :output-contract (booleanp (perm-equal a b))
    (if (not (and (listp a) (listp b)))
        (equal a b)
        (if (or (endp a) (endp b))
            (equal a b)
            (perm-equal (delete (first a) a) (delete (first a) b)))))

(check= (perm-equal 1 2) nil)
(check= (perm-equal nil nil) t)
(check= (perm-equal '(1) nil) nil)
(check= (perm-equal '(1) '(1)) t)
(check= (perm-equal '(1 2) '(1)) nil)
(check= (perm-equal '(1 2) '(2 1)) t)
(check= (perm-equal '(2 2) '(1 2)) nil)
(check= (perm-equal '(2 2) '(1 2 2)) nil)


;; num-found2-r: All x List -> Nat
;; (num-found2-r e l) takes any element e (which may be a list)
;; and a list l and determines how many occurances of e or its
;; permutations are in l (if e = l then return 1)
;; The -r indicates that the search is recursive so if e is in a
;; a sublist of l, it should be found and counted.
(defunc num-found2-r (e l)
    :input-contract (listp l)
    :output-contract (natp (num-found2-r e l))
    (cond 
        ((endp l) 0)
        ((perm-equal e l) 1)
        ((perm-equal e (first l)) (+ 1 (num-found2-r e (rest l))))
        ((listp (first l)) (+ (num-found2-r e (first l)) 
                              (num-found2-r e (rest l))))
        (t (num-found2-r e (rest l)))))

(check= (num-found2-r '(2 1 3) '(2 (1 2 3) (4 3 (1 2 3)))) 2)
(check= (num-found2-r '(2 3 1) '(2 (1 2 3) (4 3 (1 2 3 4)))) 1)
(check= (num-found2-r 2 '(2 (1 2 3) (4 3 (1 2 3 4) 2 3))) 4)
(check= (num-found2-r '(2 2) '(2 3 4 (2 4 2 2) 2 2)) 2)
(check= (num-found2-r nil '(1 (1 (1 (1 (1)))))) 0)
(check= (num-found2-r '(2 3 2) '(6 (7 2 2 3) (3 2 2) 3)) 2)
(check= (num-found2-r '(2 3 2) '()) 0)

#|
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 Part III: Depth First Search (DFS)
 OK.  Let's look at a classic computer science algorithm: depth-first
 search (or DFS).  Typically this is done for trees but it certainly
 could work with lists.  You simple need a stack (so first-in last-out)
 which *surprise* are how lists behave in a language like ACL2s.  
 So the latest element added to the stack (via a cons) is in the
 first position.
 You will do a DFS search through a list (which may have sublists), 
 looking for a number. Each time you find a sub-list you search that 
 rather than look through the rest of the main list. If you find
 the element, then you typically return the path to that element. 
 To make things easier for you to write and for us to test, instead of 
 finding a particular number, you will return the first number you 
 find that is  LESS THAN a target number. 
 Notice you will need a way to keep track of the lists/sublists you haven't
 explored yet and then you will look at the deepest unexplored list first.
 See the check= tests for example. If you are still confused PLEASE ASK.
 I have no idea if this question is too hard for you at this stage. I suspect
 it might be a challenge.
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
|#
:program

(defunc atomp (e)
    :input-contract t
    :output-contract (booleanp (atomp e))
    (not (consp e)))

(check= (atomp 1) t)
(check= (atomp (list 1 2 3)) nil)
(check= (atomp nil) t)
(check= (atomp 'a) t)

;; DEFINE
;; dfs: Rational x List -> Rational
;; (dfs r l) returns the first rational number in l
;; that is less than r using a DFS algorithm.
;; If an element is not found, return r (to indicate no value less than
;; r was found)
;; You will NOT be able to write this with a single function.
(defunc dfs (r l)
    :input-contract (and (rationalp r) (listp l))
    :output-contract (rationalp (dfs r l))
    (cond
        ((endp l) r)
    	((atomp (first l)) 
         	(if (and (rationalp (first l)) (< (first l) r)) 
                (first l)
                (dfs r (rest l))))                
    	((listp (first l))
         (if (< (dfs r (first l)) r)
             (dfs r (first l))
             (dfs r (rest l))))
    	(t r)))

(check= (dfs 1 nil) 1)
(check= (dfs 1 '(4 3 2 (1))) 1)
(check= (dfs 2 '(4 3 2 (2 1) (0))) 1)
(check= (dfs 4 '(((a 5 3)(2 4 5)) (4 5 (1 2 3)) 0)) 3)
(check= (dfs 3 '(((a 5 3)(2 4 5)) (4 5 (1 2 3)) 0)) 2)
(check= (dfs 2 '(((a 5 3)(2 4 5)) (4 5 (1 2 3)) 0)) 1)
(check= (dfs 1 '(((a b c)(d e f)) (4 5 (a b c)) f)) 1)
```




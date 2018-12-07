```lisp
#|
CS 2800 Homework 4 - Fall 2018

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
For this homework you will need to use ACL2s.

Technical instructions:

- open this file in ACL2s as hw04.lisp

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

- when done, save your file and submit it as hw04.lisp

- avoid submitting the session file (which shows your interaction with the
  theorem prover). This is not part of your solution. Only submit the lisp
  file!

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
The markers have been given permission to add any tests they want. Thus one
way to tell how many tests you need: are you positive the markers won't break
your code?

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

You should also use test? for your tests. See Section 2.13 of the
lecture notes. An example of test? is the following.

(test? (implies (and (listp l) (natp n)) 
                (<= (len (sublist-start n)) n))) 

|#


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Propositional Logic
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|

We use the following ascii character combinations to represent the Boolean
connectives:

  NOT     ~

  AND     /\ ..... ^ in the programming section
  OR      \/ ..... v in the programming section

  IMPLIES =>

  EQUIV   =
  XOR     <>

The binding powers of these functions are listed from highest to lowest
in the above table. Within one group (no blank line), the binding powers
are equal. This is the same as in class.

|#

#|
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
Section 1: Truth Tables

Construct the truth table for the following Boolean formulas. Use
alphabetical order for the variables in the formula, and create columns for
all variables occurring in the formula AND for all intermediate
subexpressions. (with one exception: you can omit negations of a single
variable such as ~p or ~r).

For example, if your formula is

(p => q) \/ r

your table should have 5 columns: 
p | q | r |  p => q |  (p => q) \/ r
-------------------------------------
F | F | F |   T     |   T
F | F | T |   T     |   T
F | T | F |   T     |   T
F | T | T |   T     |   T
T | F | F |   F     |   F
T | F | T |   F     |   T
T | T | F |   T     |   T
T | T | T |   T     |   T

Then decide whether the formula is satisfiable, unsatisfiable, valid, or
falsifiable (more than one of these predicates will hold!). 
Example: Satisfiable and falsifiable

a) (~p /\ q) = (q => p)

| p | q | ~p /\ q | q => p | (~p /\ q) = (q => p) |
| - | - | ------- | ------ | -------------------- |
| T | T | f       | T      | f                    |
| T | f | f       | T      | f                    |
| f | T | T       | f      | f                    |
| f | f | f       | T      | f                    |

falsifiable and unsatisfiable

b) (~p => q) /\ (~r => q) <> (~p => r)  
 = {p => q = ~p \/ q}
   (p \/ q) /\ (r \/ q) <> (p \/ r)
 = {Distributive}
   (p /\ (r \/ q)) \/ (q /\ (r \/ q)) <> (p \/ r)
 = {Annihilator}
   (p /\ (r \/ q)) \/ q <> (p \/ r)
 = {Distributive}
   (p \/ q) /\ ((r \/ q) \/ q) <> (p \/ r)
 = {(r \/ q \/ q) = (r \/ q)}
   (p \/ q) /\ (r \/ q) <> (p \/ r)
   
   woops, mistakes were made

faslifiable and satisfiable

| p | q | r | ~p => q | ~r => q | ~p => r | (~p => q) /\ (~r => q) | (~p => q) /\ (~r => q) <> (~p => r) |
| - | - | - | ------- | ------- | ------- | ---------------------- | ----------------------------------- |
| T | T | T | T       | T       | T       | T                      | f                                   |
| T | T | f | T       | T       | T       | T                      | f                                   |
| T | f | T | T       | T       | T       | T                      | f                                   |
| T | f | f | T       | f       | T       | f                      | T                                   |
| f | T | T | T       | T       | T       | T                      | f                                   |
| f | T | f | T       | T       | f       | T                      | T                                   |
| f | f | T | f       | T       | T       | f                      | T                                   |
| f | f | f | f       | f       | f       | f                      | f   


Hint: your table should have 5 or 8 columns (depending if you write
the not columns including columns for p,q,r).



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
Section 2. Logic Rules
Simplify the following expressions using propositional logic rules (give 
either the rule name or the formula. You will get no credit for using
a truth table.
Transformations should take the form:
   expression 1
   = {Justification}
   expression 2
   = {Justification}
   expression 3
   ......
   
After simplifying, indicate whether the expression is valid, satisfiable, falsifiable, or unsatisfiable
(there will always be 2)

(a) ~p = (q \/ p) /\ p 
  = {Absorption}
    ~p = p

The expression is unsatisfiable and falsifiable

(b) (p \/ r => ((q /\ ~r) => (r => ~r)))
  = {p => q = ~p \/ q}
    ~(p \/ r) \/ ~(q /\ ~r) \/ (~r \/ ~r))
  = {DeMorgan's and (~r \/ ~r) = ~r}
    (~p /\ ~r) \/ (~q \/ r) \/ ~r
  = {Commutative}
    (~p /\ ~r) \/ ~r \/ (~q \/ r)
  = {Absorption}
    ~r \/ (~q \/ r)
  = {Associative and Commutative}
    (~r \/ r) \/ ~q
  = {(~r \/ r) = t}
    t \/ ~q
  = {Annihilator}
	t

The expression is valid and satisfiable

(c) (~a /\ ~b) \/ (b \/ (a /\ ~b))
  = {Distributive}
    (~a /\ ~b) \/ ((b \/ a) /\ (b \/ ~b))
  = {(b \/ ~b) = t}
	(~a /\ ~b) \/ ((b \/ a) /\ t)
  = {Identity}
    (~a /\ ~b) \/ (b \/ a)
  = {DeMorgans}
	~(a \/ b) \/ (a \/ b)
  = {(~a \/ a) = t}
	t

The expressions is valid and satisfiable


(d) (p <> (q /\ r)) => ~(~p = (q /\ r))
  = {(a <> b) = ~(a = b) | ((a ~p) (b (q /\ r)))}
    (p <> (q /\ r)) => (~p <> (q /\ r))
  = {L0}
    ((p /\ (q /\ r)) \/ (~p /\ ~(q /\ r))) => ((~p /\ (q /\ r)) \/ (p /\ ~(q /\ r)))
  = {a => b = ~a \/ b}
    ~((p /\ (q /\ r)) \/ (~p /\ ~(q /\ r))) \/ ((~p /\ (q /\ r)) \/ (p /\ ~(q /\ r)))
  = {DeMorgan's}
    (~(p /\ (q /\ r)) /\ ~(~p /\ ~(q /\ r))) \/ ((~p /\ (q /\ r)) \/ (p /\ ~(q /\ r)))
  = {DeMorgan's}
    ((~p \/ ~(q /\ r)) /\ (p \/ (q /\ r))) \/ ((~p /\ (q /\ r)) \/ (p /\ ~(q /\ r)))
  = {L1}
    t

Lemma 0: a <> b = (a /\ b) \/ (~a /\ ~b)
Proof by Truth Table
| a | b | a /\ b | ~a /\ ~b | (a /\ b) \/ (~a /\ ~b) | a <> b |
| - | - | ------ | -------- | ---------------------- | ------ |
| T | T |   T    |    F     |          T             |   T    |
| T | F |   F    |    F     |          F             |   F    |
| F | T |   F    |    F     |          F             |   F    |
| F | F |   F    |    T     |          T             |   T    |

Lemma 1: 
	a <> b => ~a <> b = a <> b
  = {L0}
    (a /\ b) \/ (~a /\ ~b) => (~a /\ b) \/ (a /\ ~b)
  = {a => b = ~a \/ b}
    ~((a /\ b) \/ (~a /\ ~b)) \/ (~a /\ b) \/ (a /\ ~b)
  = {DeMorgan's}
    (~(a /\ b) /\ ~(~a /\ ~b)) \/ (~a /\ b) \/ (a /\ ~b)
  = {DeMorgan's}
    (~a \/ ~b) /\ (a \/ b) \/ (~a /\ b) \/ (a /\ ~b)
  = {Distributive}
    ((~a \/ ~b) /\ a) \/ ((~a \/ ~b) /\ b) \/ (~a /\ b) \/ (a /\ ~b)
  = {Distributive}
    ((~a /\ a) \/ (~b /\ a)) \/ ((~a /\ b) \/ (~b /\ b)) \/ (~a /\ b) \/ (a /\ ~b)
  = {Complement}
    (f \/ (~b /\ a)) \/ ((~a /\ b) \/ f) \/ (~a /\ b) \/ (a /\ ~b)
  = {Identity}
 	(a /\ ~b) \/ (~a /\ b) \/ (~a /\ b) \/ (a /\ ~b)
  = {Commutative}
    (a /\ ~b) \/ (a /\ ~b) \/ (~a /\ b) \/ (~a /\ b)
  = {a \/ a = a}
    (a /\ ~b) \/ (~a /\ b)
  = {Distributive}
	((a /\ ~b) \/ ~a) \/ ((a /\ ~b) \/ b)
  = {Distributive}
    ((a \/ ~a) /\ (~b \/ ~a)) \/ ((a \/ b) /\ (~b \/ b))
  = {Complement}
    (t /\ (~b \/ ~a)) \/ ((a \/ b) /\ t)
  = {Identity}
    ~b \/ ~a \/ a \/ b
  = {Commutative}
    a \/ ~a \/ b \/ ~b
  = {Complement}
    t \/ t
  = {t \/ t = t}
    t

The expression is valid and satisfiable. 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

|#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Section 3: Characterization of formulas
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
#|

For each of the following formulas (A through D), indicate which formula of 
formulas i to iv it is equivalent to (i to iv only match one formula each):
i) q <> q
ii) (q /\ q) \/ p
iii) ~p
iv) q

Provide proofs of your matching using a truth table or simplification
argument. For satisfiable and falsifiable formulas,  give a variable
assignment that resolves to true and one that resolves to false.

(A) ~p =  (p => (q => q))
  = {p => q = ~p \/ q}
    ~p = ~p \/ (~q \/ q)
  = {Complement}
	~p = ~p \/ t
  = {Annihilator}
	~p = t
  = {~p = t is ~p}
	~p

The expression matches iii).
When p is T, the expression is F.
When p is F, the expression is T.


(B) (p => q) = ~(q  \/ ~p)
  = {Commutative}
    (p => q) = ~(~p \/ q)
  = {p => q = ~(~p \/ q}
    (p => q) = ~(p => q)
  = {a = ~a is always false}
    nil

The expression matches i), because q <> q is always false.

(C) ((false /\ ~p) \/ ~p) => (q <> false)
  = {Annihilator}
    (false \/ ~p) => (q <> false)
  = {Identity}
    ~p => (q <> false)
  = {L1}
    ~p => q
  = {p => q = ~p \/ q | ((p ~p) (q q))}
    ~(~p) \/ q
  = {Double Negation}
    p \/ q

Lemma 1:
q = q <> false
| q | q <> false |
| - | ---------- |
| t | t          |
| f | f          |

The expression matches ii), because (q /\ q) is q. 
When p is F and q is F, the expression is F. 
When p is T and q is F, the expression is T. 


(D) [(~(~p /\ q) \/ r) /\ (p \/ ~q \/ ~r)] \/ q => (q /\ q)
  = {DeMorgans}
    [((p \/ ~q) \/ r) /\ ((p \/ ~q) \/ ~r)] \/ q => (q /\ q)
  = {L2 | ((a (p \/ ~q)) (b r))}
    (p \/ ~q) \/ q => (q /\ q)
  = {Associative}
    p \/ (~q \/ q) => (q /\ q)
  = {Complement}
    p \/ true => (q /\ q)
  = {Annihilator}
    true => (q /\ q)
  = {a => b = ~a \/ b | ((a true) (b (q /\ q)))}
    false \/ (q /\ q)
  = {Identity}
    (q /\ q)
  = {a /\ a = a | ((a q))}
    q

Lemma 2:
    (a \/ b) /\ (a \/ ~b) = a
  = {Distributive}
    ((a \/ b) /\ a) \/ ((a \/ b) /\ ~b) = a
  = {Absorption}
    a \/ ((a \/ b) /\ ~b) = a
  = {Distributive}
    a \/ ((a /\ ~b) \/ (b /\ ~b)) = a
  = {Complement}
    a \/ ((a /\ ~b) \/ false) = a
  = {Identity}
    a \/ (a /\ ~b) = a
  = {Absorption}
	a = a 
  = {QED}

The expression matches iv).
When q is T, the expression is T
When q is F, the expression is F

|#



#|
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
SECTION 4: Truth Tables

In previous terms we've looked at satisfiability in this assignment.  It's a known
Non-deterministic Polynomial Time problem (AKA bad bad bad) that is a real costraint 
in a variety of CS domains. In fact, if you can solve this issue in polynomial time
you can earn a million dollars and go down in history as one of the all time great 
computer scientists.....so pretty simple for a weekly assignment.

So why is satisfiability so difficult to prove.  Well you try out all assignment
combinations for all variables and see if one of them resolves to true.....what if we
keep going?  What if we make a truth table for all variable assignments?

This term we'll try to generate the truth table. We're even going
to give you some code to make your life easier.

NOTE: I got lazy and was sick of waiting for ACL2s to prove things.  Getting the
theorem prover to get all these theorems in is just annoying. :)  Thus program mode.
Don't judge me too harshly and please make sure your code follows the design
recipe and is based on the data definitions.

If you define a recursive function based on a recursive data
definition, use the template that the data definition gives rise
to. Read Section 2.15 of the lecture notes and make sure to at
least read up to the bottom of page 25.  Notice that in the
definition of foo in the lecture notes, you have TWO recursive
calls in a single cond branch, due to the PropEx data definition. 
Even if you don't use program mode, I've turned off the strict 
error checking (it will check but then time out in some cases) to
hopefully make your life easier. Just the same, please be patient 
regarding the existing code being admitted into ACL2s.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
|#

;; Let's define the operators for any propositional expression
(defdata UnaryOp '~)
; BinaryOp: '^ means "and", 'v means "or", '<> means "xor"
; and '=> means "implies" 
(defdata BinaryOp (enum '(^ v <> =>)))

; PXVar are the set of possible variables in an expression.
(defdata PXVar (enum '(a b c d e f g h i j k p q r s x y z)))

; PropEx: A Propositional Expression (PropEx) can be a boolean (t
; or nil), a PXVar denoting a variable (e.g. 'p or 'q), or a
; list denoting a unary or binary operation. 
(defdata (BinEx (list BinaryOp PropEx PropEx))
  (UnaryEx (list UnaryOp PropEx))
  (PropEx (oneof boolean PXVar UnaryEx BinEx)))


; A list of prop-vars (PXVar))
(defdata Lopv (listof PXVar))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; GIVEN
;; add: PXVar x lopv -> lopv
;; (add a X) conses PXVar a to list of PXVars
;; X if and only if a is not in X
;; You can use the function in.
(defunc add (a X)
  :input-contract (and (PXVarp a)(lopvp X))
  :output-contract (lopvp (add a X))
  (if (in a X)
    X
    (cons a X)))

(check= (PropExp '(v t (=> s q))) t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; del: Any x list -> list
;; Del removes the first instance of an element e in list l
(defunc del (e l)
  :input-contract (listp l)
  :output-contract (listp (del e l))
  (cond ((endp l) l)
        ((equal e (first l)) (rest l))
        (t (cons (first l) (del e (rest l))))))

;; IGNORE (for proofs)
(sig del (all (listof :b)) => (listof :b))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; GIVEN
;; perm: list x list -> boolean
;; We define perm (permutation) to make the tests more robust. 
;; Otherwise, some of the tests below may fail because the order 
;; in which variables appear does not matter. 
(defunc perm (a b)
  :input-contract (and (listp a) (listp b))
  :output-contract (booleanp (perm a b))
  (cond ((endp a) (endp b))
        ((endp b) nil)
        (t (and (in (first a) b)
                (perm (rest a) (del (first a) b))))))

;; BRUTE FORCE: I was sick of waiting for ACL2s to prove things or trying to get
;; the theorem prover to work. Hence I'm cheating and putting things in program mode.
;; That's OK but please convince yourself that your code works, terminates, and makes sense.
:program

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; get-vars: PropEx x LOPV -> LOPV 
;; get-vars returns a list of variables appearing in px OR
;;   in the provided accumulator acc. If acc has
;;   no duplicates in it, then the returned list should not have
;;   any duplicates either. See the check='s below.
(defunc get-vars (px acc)
  :input-contract (and (PropExp px) (Lopvp acc))
  :output-contract (Lopvp (get-vars px acc))
  (cond ((booleanp px) acc)
        ((PXVarp px) (add px acc))
        ((UnaryExp px)(get-vars (second px) acc))
        (t (get-vars (third px)
                     (get-vars (second px) acc)))))


(check= (get-vars '(v r (=> s q)) nil) '(q s r))
(check= (get-vars '(v t nil) nil) nil)
(check= (get-vars '(v s (^ s b)) nil) '(b s))
(check= (get-vars '(v s (~ b)) '(c)) '(b s c))

(check= (perm (get-vars 'A '()) '(A)) t)
(check= (perm (get-vars 'A '(B C)) '(A B C)) t)
(check= (perm (get-vars '(^ A B) '()) '(B A)) t)
(check= (perm (get-vars '(^ B A) '()) '(B A)) t)


;; We need to evaluate all possible ways variables can be
;; assigned.  Thus we need lists of var-value pairs (assign-lists)
;; ...and lists of these lists (loal)
(defdata PXvar-val (list PXVar boolean))
(defdata assign-list (listof PXvar-val))
(defdata loal (listof assign-list))
(defdata al-val (list assign-list boolean))
(defdata loav (listof al-val))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; DEFINE
;; update: PropEx x PXVar x Boolean -> PropEx
;; The update function "updates a variable" by replacing all instances
;; of the variable with the boolean val in the expression px.
;; Use the template propex gives rise to, as per the lecture notes.
;; Look at get-vars (above).
(defunc update (px name val)
    :input-contract (and (PropExp px) (PXVarp name) (booleanp val))
    :output-contract (PropExp (update px name val))
    (cond
        ((booleanp px) px)
        ((PXVarp px) (if (equal name px) val px))
        ((UnaryExp px) (list (first px) (update (second px) name val)))
        ((BinExp px) (list (first px) 
                              (update (second px) name val)
                              (update (third px) name val)))
        (t px)))
                       

(check= (update T 'A NIL) T)
(check= (update NIL 'A T) NIL)
(check= (update 'A 'B T) 'A)
(check= (update 'A 'A NIL) NIL)
(check= (update '(^ (v NIL A) (~ B)) 'A T) '(^ (v NIL T) (~ B)))
(check= (update '(^ (v NIL A) (~ B)) 'C T) '(^ (v NIL A) (~ B)))
(check= (update '(^ (v NIL A) (~ B)) 'B NIL) '(^ (v NIL A) (~ NIL)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; GIVEN
;; ConstBoolExp: All -> Boolean
;; (ConstBoolExp px) determines if px is a PropEx 
;; with NO symbols / free variables.
(defunc ConstBoolExp (px)
  :input-contract t
  :output-contract (booleanp (ConstBoolExp px))
  (and (PropExp px) (equal (get-vars px nil) nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; GIVEN
;; xor: Boolean x Boolean -> Boolean
;; (xor x y) returns true iff x or y are true but
;; not both
(defunc xor (x y)
  :input-contract (and (booleanp x)(booleanp y))
  :output-contract (booleanp (xor x y))
  (if x (not y) y))


;; DEFINE (helper)
;; update-all: PropEx x assign-list -> PropEx (presumably a ConstBoolExp)
;; (update-all px lv) takes a propex and updates it with each var-value assignment
;; given. This should make the resultant px a ConstBoolEx but we still need to test this.
(defunc update-all (px al)
    :input-contract (and (PropExp px) (assign-listp al))
    :output-contract (PropExp (update-all px al))
    (if (or (ConstBoolExp px) (endp al)) 
        px
        (update-all (update px (first (first al)) (second (first al)))
                    (rest al))))

(check= (update-all '(^ A (^ B C)) '((A T) (B NIL) (C NIL))) '(^ T (^ NIL NIL)))
(check= (update-all '(^ A (^ A A)) '((A T) (B NIL) (C NIL))) '(^ T (^ T T)))
(check= (update-all 'A '((A T))) T)
(check= (update-all T '((A T))) T)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; HELPER
;; apply: BinOp Boolean Boolean -> Boolean
;; Applies the given binary operation to the arguments b1 and b2
(defunc apply (op b1 b2)
    :input-contract (and (BinaryOpp op) (booleanp b1) (booleanp b2))
    :output-contract (booleanp (apply op b1 b2))
    (cond 
        ((equal op '^) (and b1 b2))
        ((equal op 'v) (or b1 b2))
        ((equal op '<>) (xor b1 b2))
        ((equal op '=>) (or (not b1) b2))
        (t t)))

(check= (apply '^ T T) T)
(check= (apply 'v NIL NIL) NIL)
(check= (apply '=> NIL NIL) T)
(check= (apply '<> T NIL) T)



;; DEFINE
;; beval: PropEx -> Booleanp
;; beval evaluates a constant boolean expression and returns its value.  
;; A constant boolean expression is a PropEx with no variables, 
;; just booleans and operators. If this expression has variables, then
;; return nil. You may have to define helper functions to evaluate
;; your expressions.
(defunc beval (bx)
	:input-contract (PropExp bx)
    :output-contract (booleanp (beval bx))
    (if (not (ConstBoolExp bx))
        nil
        (cond
            ((PXVarp bx) nil)
            ((booleanp bx) bx)
            ((UnaryExp bx) (not (beval (second bx))))
            (t (apply (first bx) (beval (second bx)) (beval (third bx)))))))


(check= (beval T) T)
(check= (beval NIL) NIL)
(check= (beval '(^ T NIL)) NIL)
(check= (beval '(^ T T)) T)

;; More checks
(check= (beval 'B) NIL)
(check= (beval '(<> T NIL)) T)
(check= (beval '(=> T T)) T)
(check= (beval '(=> T (^ NIL F))) NIL)


;; Tests that may help you later (you can also write your own)
(defconst *test_px1* '(v (~ a) (^ a c))) ;; Sat/Falsifiable
(defconst *test_px2* '(v (~ a) (^ b c))) ;; Sat/Falsifiable
(defconst *test_px3* '(=> t (^ b (~ b)))) ;; Unsatisfiable/Falsifiable
(defconst *test_px4* '(<> (~ a) a)) ;; Valid/Satisfiable
(defconst *test_px5* t)
(defconst *test_px6* nil)
(defconst *test_px7* '(=> a (^ a (v a b)))) ;; Valid/Satisfiable


(defconst *test_px2assign1* '((a 0) (b 0) (c 0)))
(defconst *test_px2assign2* '((a 1) (b 0) (c 0)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
#|
 Generating the Truth Table:
 1) Find all the variables in the expression
 2) Generate all ways the variables can be assigned t/nil. Each
    way the variables can be assigned is a assign-list (list of var-value pairs)
    and a assign-list defines a unique ASSIGNMENT SET.
 3) Assign the variables in the prop ex using each assignment set
    to make a constant boolean expression.
 3) Evaluate each constant boolean expression you generate
    (using beval) and pair that with the assignment set. 
 4) By outputting a loav the TT-print function essentially spits out
 a truth table (the formatting may be ugly but we'll ignore this).
|#

;; HELPER
;; gen : PXVar x LOAL -> LOAL
;; (gen a set) generates all possible combinations of a and an existing 
;; assignment set
(defunc gen (var set)
    :input-contract (and (PXVarp var) (loalp set))
    :output-contract (loalp (gen var set))
    (if (endp set)
        (list (list (list var T))
              (list (list var NIL)))
        (cons (cons (list var T) (first set))
        (cons (cons (list var NIL) (first set))
              (if (endp (rest set)) '() (gen var (rest set)))))))




(check= (gen 'A '()) (LIST '((A T))
                           '((A NIL))))
(check= (gen 'B (LIST '((A T))
                      '((A NIL))))
        (List '((B T) (A T))
              '((B NIL) (A T))
              '((B T) (A NIL))
              '((B NIL) (A NIL))))

;; DEFINE
;; genVarAssignSet: LOPV x LOAL -> LOAL
;; genVarAssignSet generates a unique assignment set for each variable
;; in the list of variables (vars) by using an accumulator for the assignment
;; sets generated so far.
(defunc genVarAssignSet (vars acc) 
    :input-contract (and (lopvp vars) (loalp acc))
    :output-contract (loalp (genVarAssignSet vars acc))
    (if (endp vars) acc
        (genVarAssignSet (rest vars) (gen (first vars) acc))))

(check= (genVarAssignSet '() '()) '())
(check= (genVarAssignSet '(A) '()) (LIST '((A T))
                                         '((A NIL))))
(check= (genVarAssignSet '(A B) '()) (List '((B T) (A T))
              							   '((B NIL) (A T))
              							   '((B T) (A NIL))
             							   '((B NIL) (A NIL))))


;; HELPER
;; getTTHlp : PropEx LOAL LOAV -> LOAV
;; (getTTHlp px loSet acc) takes a prop expression and a list of assignment sets
;; and tries to apply each assignment set to the prop expression. The resulting
;; output is stored in acc.
(defunc getTTHlp (px loSet acc)
    :input-contract (and (PropExp px) (loalp loSet) (loavp acc))
    :output-contract (loavp (getTTHlp px loSet acc))
    (if (endp loSet) acc
        (let* ((a-set (first loSet))
               (bx (update-all px a-set))
               (value (beval bx))
 		       (a-set-result (list a-set value)))    	
	        (getTTHlp px (rest loSet) (cons a-set-result acc)))))

(check= (getTTHlp *test_px1* (LIST '((A T) (C T))) '())
	    (list (list '((A T) (C T)) T)))
(check= (getTTHlp *test_px1* (LIST '((A T) (C NIL))) '())
	    (list (list '((A T) (C NIL)) NIL)))
(check= (getTTHlp *test_px1* (LIST '((A T) (C NIL)) '((A T) (C T))) '())
	    (list (list '((A T) (C T)) T)
	          (list '((A T) (C NIL)) NIL)))


;; getTT: PropEx -> LOAV
;; (getTT px) takes a prop expression and returns a list of assignment values
;; that "LOOK NICE".  The check= tests below follows the output format we are expecting
;; with each row having a set of variable assignment pairs and a final value
;; the expression evaluates to.
(defunc getTT (px)
  :input-contract (PropExp px)
  :output-contract (loavp (getTT px))
  (getTTHlp px (genVarAssignSet (get-vars px nil) nil) nil))

(check= (getTT *test_px1*)
        (LIST (LIST '((A nil) (C nil)) t)
              (LIST '((A t) (C nil)) nil)
              (LIST '((A nil) (C t)) t)
              (LIST '((A t) (C t)) t)))

(check= (getTT *test_px2*)
        (LIST (LIST (LIST (LIST 'A NIL)
                          (LIST 'B NIL)
                          (LIST 'C NIL))
                    T)
              (LIST (LIST (LIST 'A T)
                          (LIST 'B NIL)
                          (LIST 'C NIL))
                    NIL)
              (LIST (LIST (LIST 'A NIL)
                          (LIST 'B T)
                          (LIST 'C NIL))
                    T)
              (LIST (LIST (LIST 'A T)
                          (LIST 'B T)
                          (LIST 'C NIL))
                    NIL)
              (LIST (LIST (LIST 'A NIL)
                          (LIST 'B NIL)
                          (LIST 'C T))
                    T)
              (LIST (LIST (LIST 'A T)
                          (LIST 'B NIL)
                          (LIST 'C T))
                    NIL)
              (LIST (LIST (LIST 'A NIL)
                          (LIST 'B T)
                          (LIST 'C T))
                    T)
              (LIST (LIST (LIST 'A T)
                          (LIST 'B T)
                          (LIST 'C T))
                    T)))

(check= (getTT *test_px3*) 
        (LIST (LIST (LIST (LIST 'B NIL)) NIL)
              (LIST (LIST (LIST 'B T)) NIL)))

(check= (getTT *test_px4*) 
        (LIST (LIST '((A nil)) t)
              (LIST '((A t)) t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; SECTION 5: SPECIFICATIONS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|

This section deals with specifications using the functions
defined in the lecture notes. Claims below may not satisfy contract 
checking, which means you should perform contract completion before
answering the question.

For each claim, formalize it using ACL2s syntax (remember this requires
that you perform contract completion).

After that, indicate whether the formalized claim is valid.  You
do not need proofs here. If it is invalid, provide a
counterexample. Also, if it is invalid, propose the simplest
modification you can think of that is valid and formalize your
new claim. For this part, there may be many possible answers, in
which case any reasonable answer is fine.  If you discover
anything interesting, indicate it.

Example: len2 is equivalent to len where len2 is defined as

(defunc len2 (l)
  :input-contract (listp l)
  :output-contract (natp (len2 l))
  (if (endp l)
    0
    (+ 1 (len2 (rest l)))))

Answer.
The formalization is:

(implies (listp l)
         (equal (len2 l) (len l)))


This conjuncture is valid.

Note: that if you were to say that the claim is false because it
only holds for lists, that is not a correct answer because you
have to perform contract completion first!

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

5a. The length of the list obtained by consing x and y is 
    equal to the length of x plus the length of y.

(implies (and (listp x) (listp y))
         (equal (len (cons x y)) (+ (len x) (len y))))

The conjecture is falsifiable. 
With ((a (list 1 2 3)) (b (list 3 4 5)))
the result of (cons (list 1 2 3) (list 3 4 5)) has a len of 2, not 6.
Instead of using cons, using app would fix this conjecture.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
5b. If an element e is in the list generated by appending lists x and y, then
e must be in list x and list y.

(implies (and (listp x) (listp y) (in e (append x y)))
         (and (in e x) (in e y)))

The conjecture is falsifiable
With ((e 2) (x (list 1 2 3)) (y (list 4 5 6)))
e is in (append x y), and in x, but it is not in y.
The conjecture should state that e is in x or y, not both x and y.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
5c. For any propositional expression (defined above), if the expression
is not a cons then it is not a unary expression or a binary expression

(implies (and (PropExp p) (not (consp p))
         (not (or (UnaryExp p) (BinExp p)))

The conjecture is valid. PropEx can be oneof BinEx, UnaryEx, PXVar, or boolean. Since BinEx and UnaryEx are both list-type data, PropEx can only be one of PXVar or boolean if its not a cons.


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
5d. If I add (using that function name) an element e to a list l and then
delete e from the resultant list, then e is not in l (since add doesn't
add an item if already in the list).

(implies (listp l) 
		 (not (in e (del e (add e l))))

The conjecture is falsifiable
If l is a list a several e's, let's say 10, then the program will only delete one of the e's, but not all of them. 
In order for the conjecture to be true, the list must contain at most 1 e. 


|#
```

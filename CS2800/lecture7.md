## Lecture 7: Propositional Logic

#### Data Definitions (Cont.)

```lisp
(defdata file (list string nat))
(defdata 
    (dir (list string dir-file-list))
    (dir-file-list (listof file-dir))
    (file-dir (oneof file dir)))

#| Examples |#
(defconst *cs2800-dir*
    '("cs2800" (("hw2.lisp" 12) ("hw3.lisp" 34))))

(dirp `("classes" (,*cs2800-dir* 
                    ("cs2500" (("hw2.lisp" 92))))))
(defunc find-file (f fd)
    :input-contract (and (filep f) (file-dirp fd))
    :output-contract (booleanp (find-file f fd)) 
    (cond ((filep fd) (equal f fd)
          ((endp (second fd)) nil)
          (t (or (find-file f (first (second fd)))
                 (find-file f (list (first fd)
                                    (rest (second fd)))))))))
```

#### Truth Tables

| p    | q    | p ^ q | p v q | p -> q | ~p   | ~p v q |
| ---- | ---- | ----- | ----- | ------ | ---- | ------ |
| f    | f    | f     | f     | t      | t    | t      |
| f    | t    | f     | t     | t      | t    | t      |
| t    | f    | f     | t     | f      | f    | f      |
| t    | t    | t     | t     | t      | f    | t      |

**To Prove ->**

- Assume LHS, prove RHS

- |          p |  ->  | q          |
  | ---------: | :--: | ---------- |
  | Antecedent |      | Consequent |

**To use ->**

- Prove the LHS, and conclude RHS (Modus Ponens)

(p -> q) = (~p v q)

| p    | q    | p XOR q | p = q |
| ---- | ---- | ------- | ----- |
| f    | f    | f       | t     |
| f    | t    | t       | f     |
| t    | f    | t       | f     |
| t    | t    | f       | t     |

(p XOR q) = ~(p = q)

| Real     | ASCII | Not OK |
| -------- | ----- | ------ |
| NOT      | ~     |        |
| AND      | /\    | &&     |
| OR       | \/    | \| \|  |
| ->       | =>    |        |
| XOR      | <>    |        |
| <u>=</u> | =     |        |




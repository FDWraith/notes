## Lecture 6: Data Definitions

#### Constant Definitions

```lisp
(defconst *mydef* 6) ; Astericks needed 
(+ *mydef* 1)        ; Using the constant
```

#### Data Definitions

Singleton Types

```lisp
#| Singleton types allow us to define types that contain only one object |#
(defdata one 1)
(defdata instr 'David)

#| All data definitions give rise to a recognizer |#
(onep 1)
(instrp 'David)
```

Enumerated Types

```lisp
#| Enumerated types allows you to define finite types |#
(defdata name (enum '(pete ken david)))

(thm (implies (namep x)
              (or (equal x 'pete)
                  (equal x 'ken)
                  (equal x 'david))))
```

Range Types

```lisp
#| Range types allwo you to define a range of numbers. |#
(defdata probability (range rational (0 <= _ <= 1)))
(defdata big-nat (range integer ((* 1024 1024) < _))) 

; Excluding part of the range automatically puts infinity by default
```

Product Types

```lisp
#| Product types allow us to define structured data |#
(defdata fullname (list string string probability))

#| Records are product types, wher the fields are named |#
(defdata fullname-rec (record (first . string)
                              (last . string)
                              (numcomics . nat)))
#| Records require a constructor |#
(fullname-rec "David" "Sprague" 937)
(fullname-rec-first (fullname-rec "David" "Sprague" 937))
```

List Types

```lisp
#| List types can be made using listof combinator |#
(defdata lop (listof probability))
```

Union Types

```lisp
#| Union types let us combine types |#
(defdata intstr (oneof integer string))
```

Recursive Types

```lisp
#| Recursive type expressions can reference themselves |#
(defdata loint (oneof nil (cons integer loint)))
```

#### Depth-First Search

```lisp
; If there is an element f < e, dfs should not return e
(test? (implies (and (rationalp e) (lorp l) (consp l) (rationalp f) (< f e) (in f l)) 
                (not (equal (dfs e l) e))))

; If there is an element r > e, dfs should not return r
(test? (implies (and (rationalp r) (rationalp e) (lorp l) (in r l) (> r e))
                (not (equal r (dfs e l)))))


(test? (implies (and (natp e) (listp l) (rationalp f) (< f e) (not (in f l)))
                (equal (dfs e l) e)))

; If dfs finds an element in l1, it should find the same element if l1 is the first list in a new list.
(test? (implies (and (rationalp e) (listp l1) (< (dfs e l1) e) (listp l2))
                (equal (dfs e (list l1 l2)) (dfs e l1)))
```

```lisp
(defunc count-ops (px)
    :input-contract (PropExp px)
    :output-contract (natp (count-ops px))
    (cond ((booleanp px) 0)
          ((pvarp px) 0)
	      ((and (consp px) (UnaryOpp (first px))) (+ 1 (count-ops (rest px))))
          ((and (consp px) (BinaryOpp (first px))) 
           (+ 1 (count-ops (second px)) (count-ops (third px))))
          (t 0)))

```






































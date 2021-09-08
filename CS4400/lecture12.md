## More Lambda Calculus

### Numbers + Booleans

```
if-else := λb t f. b t f

is-zero := λn. n (λx. false) true

one := λs z. s z
two := λs z. s (s z)

pred := λn. right (n (λp. pair (succ (left p)) (left p)) (pair zero zero))

minus := λm n. n pred m
plus := λm n. m succ n
times := λm n. m (plus n) zero

```

### Recursion

```
f := λf. ...

x is such x that
fix f = f (fix f)

Fixed-point combinator: find an x = fix f, such that 
 f x = x
 
-----

-- continually applies g until omega iss supplied.
Y g := (λf. (x. f (x x)) (λx. f (x x))) g

```


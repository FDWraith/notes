### Lecture 8: P = NP ?

#### From Last Time

```
p => q    =    ~p v q
p <> q    =   ~(p = q)
```

#### DeMorgan's

```
~(p ^ q)  =  ~p v ~q
~(p v q)  =  ~p ^ ~q
```

#### Distributive

```
(p ^ q) v r       =     (p v r) ^ (q v r)
(p v q) ^ r       =     (p ^ r) v (q ^ r)
p => (a ^ b)      =     (p => a) ^ (p => b)
```

#### Commutative

```
a ^ b         =          b ^ a
a v b v c     =      c v b v a
```

#### Associative

```
(a ^ b) ^ c   =  a ^ (b ^ c)
(a v b) v c   =  a v (b v c)
```

#### Identity

```
p v false     =  p
p ^ true      =  p
```

#### Annihilator

```
p ^ false     = false
p v true      = true
```

#### Absorption

```
p ^ (p v q)   =    p
p v (p ^ q)   =    p
```

#### Sample Problem

```
    p ^ (q v ~p) => ~q ^ ~p
=   {Distributive}
    (p ^ q) v (p ^ ~p) => ~q ^ ~p
=   {Complement}
    (p ^ q) v false => ~q ^ ~p
=   {Identity}
	(p ^ q) => ~q ^ ~p
=   {Def of Implies}
	~(p ^ q) v (~q ^ ~p)
=   {DeMorgans}
    (~p v ~q) v (~q ^ ~p)
=   {Associative}
	~p v (~q v (~q ^ ~p))
=   {Absorption}
	~p v ~q
```

#### Theorem Proving

| Unsatisfiable | <- Falsifiable | Satisfiable -> | Valid  |
| ------------- | -------------- | -------------- | ------ |
| false         | p              | p              | true   |
| p ^ ~p        | p ^ q          | p ^ q          | p v ~p |


### Programming in Pure Lambda Calculus

Multiple arguments

$(\lambda x. (\lambda y. y \space x))$

Currying

$((\lambda x. \lambda y. y \space x)) \rightarrow$



 **Booleans**

- values  : lambda * closed expression -- meaning no free variables
- true and false
- need two different values:
  - true: $(\lambda x \space y. x)$
  - false: $(\lambda x \space y. y)$

- if conditional: if-else
  - $(\lambda b \space t \space f. b \space  t \space f)$
- and 
  - $(\lambda x \space y. ((\lambda b \space t \space f. b \space t \space f ) \space x \space y \space \#F))$
- or
  - $(\lambda x \space y. x \space \#T \space y)$
- not
  - $(\lambda x. x \space \#F \space \#T)$

**Pairs**

- pair := $(\lambda l \space r \space s. s \space l \space r)$ 
- left := $(\lambda p. p \space (\lambda x \space y. x))$
- right :=$(\lambda p. p \space (\lambda x \space y. y))$

**Numbers**

- $(\lambda s \space z. s \space (s \space (s \space \text{... n times ...} \space z)))$
- zero := $(\lambda s \space z. z)$
- one := $(\lambda s \space z. s \space z)$
- two := $(\lambda s \space z. s \space (s \space z))$


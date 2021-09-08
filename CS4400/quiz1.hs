data Definition = Assign String Expr

data NonEmptyDefintions = [Definition]
                        | (cons Definition NonEmptyDefinitions)

data Expr = Integer Integer
          | Var String
          | Add Expr Expr
          | Neg Expr 
          | With Expr NonEmptyDefinitions

(With (Add (Neg (Var "z")) (Var "x"))
      [(Assign "x" (Integer 12)),
       (Assign "z" (Add (Integer 12) (Integer 13)))])

{-

Call By Name

-}


{-

Free Variables:
Leftmost y, Leftmost w, and y and x at the end.

Bound variables:
Leftmost x, w and z at the end.

-}

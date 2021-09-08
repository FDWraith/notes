{- |
Module      :  Assignment04
Description :  Assignment 4 submission for CS 4400.
Copyright   :  (c) Kevin Zhang

Maintainer  :  zhang.kevi@northeastern.edu
-}

module Assignment04 where

import Syntax
import Eval
import qualified SExpression as S

main :: IO()
main = do
  test_eval
  test_subst
  test_runSExpression
  test_toSExpression
  test_fromSExpression
  test_condsToSExpression
  test_fromConditionals
  test_valueToSExpression
  test_eval_if


{--

A. Could any cond expression be expressed using just if expressions?

Yes. Several cond statements can be chained together with nested-if statements, such that cond check
belongs in the else case of the previous if statement. For the cond statement that doesn't have an Else statement,
the if-statement can simply return Nothing.


B. One could say that the else clause is redundant. That is, if only the first form of cond (without the extra else clause) was available, we could we still express
```
(cond ((= x 5432) 1)
      ((= x #t) 0)
      (else -1))
```
without resorting to negating all the previous conditions. How?

We can simply express the last case as an always truthy case, like so:
(cond ((= x 5432) 1)
      ((= x #t) 0)
      (#t -1))
--}

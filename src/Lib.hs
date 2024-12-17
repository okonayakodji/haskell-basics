module Lib
  ( fact,
    factRec,
    factWithTailOpt,
    doubleFact,
  )
where

fact :: Integer -> Integer
fact 0 = 1
fact n = product [1 .. n]

factRec :: Integer -> Integer
factRec 0 = 1
factRec n = n * factRec (n - 1)

factWithTailOpt :: Integer -> Integer
factWithTailOpt n = factWithTailOpt' n 1
  where
    factWithTailOpt' a acc
      | a <= 1 = acc
      | otherwise = factWithTailOpt' (a - 1) (a * acc)

doubleFact :: Integer -> Integer
doubleFact x = f 1 x
  where
    f acc 0 = acc
    f acc 1 = acc
    f acc x = f (acc * x) (x - 2)

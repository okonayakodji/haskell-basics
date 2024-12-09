module Lib
  ( fact,
    factRec,
    factWithTailOpt,
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

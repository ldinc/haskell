{- |
 - Module : MyMath.hs  <File name or $Header$ to be replaced automatically>
 - Description : Simple math lib for practice
 - Copyright : (c) ldinc "drogunov.igor@gmail.com"
 - License : <license>
 -
 - Maintainer : <email>
 - Stability : experimental
 - Portability : portable | non-portable (<reason>)
 -
 - <module description starting at first column>
 - -}

module MyMath where

-------------------------------------------------------------------------------
{- function for calculation of Fibonacci number -}
-------------------------------------------------------------------------------
fib::Integral a => a -> a

fib n
	| n < 0 = fib (n + 2) - fib (n + 1)
	| n == 0 = 0
	| n == 1 = 1
	| otherwise = fib (n - 1) + fib (n - 2)
-------------------------------------------------------------------------------

-------------------------------------------------------------------------------
{- function for calculation list of Fibonacci number from fib(m) to fib(n) -}
-------------------------------------------------------------------------------
fiblist m n
	| n == 0 = []
	| otherwise = fiblist m (n - 1) ++ [fib (n + m - 1)]
-------------------------------------------------------------------------------

-------------------------------------------------------------------------------
{- primality test -}
-------------------------------------------------------------------------------
primality::Integral a => a -> Bool

primality n = test n 2
	where
		test x y
			| x == y = True
			| mod x y == 0 = False
			| otherwise = test x (y + 1)

-------------------------------------------------------------------------------
{- return n-th primal number -}
-------------------------------------------------------------------------------
primal::Integral a => a -> a

primal n = test 2 n
	where
		test x n
			| primality x && n == 1 = x
			| primality x == True = test (x + 1) (n - 1)
			| otherwise = test (x + 1) n
-------------------------------------------------------------------------------

-------------------------------------------------------------------------------
{- list of primal numbers from m-th to (m+n)-th -}
-------------------------------------------------------------------------------
primalList::Integral a => a -> a -> [a]

primalList n m
	| m == 0 = []
	| otherwise = map (\x -> primal x) [n .. (m+n)]		
 
-------------------------------------------------------------------------------

-------------------------------------------------------------------------------
{- range functions -}
-------------------------------------------------------------------------------
range::Integral a => a -> a -> a -> [a]

range a b d
	| a >= b = []
	| otherwise = a:range (a + d) b d

rangef::Real a => a -> a -> a -> [a]

rangef a b d
	| a >= b = []
	| otherwise = a:rangef (a + d) b d
-------------------------------------------------------------------------------

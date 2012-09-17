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
{- calculation of local minimum for f(x) using golden ratio -}
-------------------------------------------------------------------------------
--simple local minimum
locminGR::(Ord a, Floating a) => (a -> a) -> a -> a -> a -> [a]

locminGR f l r eps
	| abs(x2 - x1) <= eps = [x, (f x)]
	| f x1 < f x2  = locminGR f l x2 eps
	| otherwise = locminGR f x1 r eps
	where
		x1 = l + (3 - (sqrt 5)) * 0.5 * (r - l)
		x2 = l + ((sqrt 5) - 1) * 0.5 * (r - l)
		x = (x2 + x1) * 0.5

--bounded depth
locminGRbounded::(Ord a, Floating a) => (a -> a) -> a -> a -> a -> a -> [a]

locminGRbounded f l r eps n
        | abs(x2 - x1) <= eps = [x, (f x)]
	| n == 0 = [x, (f x)]
        | f x1 < f x2  = locminGRbounded f l x2 eps (n - 1)
        | otherwise = locminGRbounded f x1 r eps (n - 1)
	where
                x1 = l + (3 - (sqrt 5)) * 0.5 * (r - l)
                x2 = l + ((sqrt 5) - 1) * 0.5 * (r - l)
                x = (x2 + x1) * 0.5

--for debugging maybe =)
locminGRex::(Ord a, Floating a) => (a -> a) -> a -> a -> a ->IO ()

locminGRex f l r eps
	| abs(x2 - x1) <= eps = print "Result:" >> print [x, f(x)]
	| f x1 < f x2 = do
		print ([l, x2])
		(locminGRex f l x2 eps)
	| otherwise = do
		print ([x1, r])
		(locminGRex f x1 r eps)
	where
		x1 = l + (3 - (sqrt 5)) * 0.5 * (r - l)
                x2 = l + ((sqrt 5) - 1) * 0.5 * (r - l)
                x = (x2 + x1) * 0.5
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

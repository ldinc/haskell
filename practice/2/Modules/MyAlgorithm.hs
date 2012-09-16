{-
 - Module : MyAlgorithm.hs
 - Description : Simple algo lib for practice
 - Copyright : (c) ldinc "drogunov.igor@gmail.com"
 - License : <license>
 -
 - Maintainer : <email>
 - Stability : experimental
 - Portability : portable | non-portable (<reason>)
 -
 - <module description starting at first column>
 - -}

module MyAlgorithm where

-------------------------------------------------------------------------------
{- The quick sort fucntion. Args : list, compare function -}
{- Compare function a < b : a->a->Bool -}
-------------------------------------------------------------------------------
qsort::[a]->(a->a->Bool)->[a]

qsort [] cmp = []
qsort (p:xs) cmp = lesser ++ [p] ++ greater
	where
		lesser =  qsort [x|x<-xs, cmp x p == True] cmp
		greater = qsort [x|x<-xs, cmp x p /= True] cmp
-------------------------------------------------------------------------------

-------------------------------------------------------------------------------
{- The Mid function. Returning sublist from m-st pos with lenght = n  -}
-------------------------------------------------------------------------------
mid::[a] -> Int -> Int -> [a]

mid list m n
	| n == 0 = []
	| otherwise = (mid list m (n - 1)) ++ [list!!n]
-------------------------------------------------------------------------------


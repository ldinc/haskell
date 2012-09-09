{- |
Module      :  <File name or $Header$ to be replaced automatically>
Description :  <optional short text displayed on contents page>
Copyright   :  (c) <Authors or Affiliations>
License     :  <license>

Maintainer  :  <email>
Stability   :  unstable | experimental | provisional | stable | frozen
Portability :  portable | non-portable (<reason>)

<module description starting at first column>
-}


module Main where

main = print (mul 1333 3847) 

{-
	Idea:
	a,b >= 0
	a > b
	mul(a,b)	= 0, b = 0
				= a, b = 1
				= a + mul(2*a, b/2), b mod 2 = 0
				= a + mul(a, b - 1), otherwise

-}

mul a b	| abs a < abs b = (mult b $abs a) * sign a
		| otherwise = (mult a $abs b) * sign b
	where
		mult a b	| b == 0 = 0
					| b == 1 = a
					| mod b 2 == 1 = a + mult (a + a) (div b 2)
					| otherwise = mult (a+a) (div b 2) 
					

twicei x = x + x

twiced x = div x 2

sign a	| a < 0 = -1
		| otherwise = 1

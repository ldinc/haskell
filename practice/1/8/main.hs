{-
	main.hs
-}

module Main where

main::IO ()

--main = print $locmin lin 1 10 0.0001 

main = print $locmin func 1 2 0.0000000000000001

locmin (f) l r eps 
	| abs (x2 - x1) <= eps = [x, (f x)]
	| f x1 < f x2 = locmin f l x2 eps
	| otherwise = locmin f x1 r eps
	where
		x1 = l + (3 - (sqrt 5)) * 0.5 * (r - l)
		x2 = l + ((sqrt 5) - 1) * 0.5 * (r - l)
		x = (x2 + x1) * 0.5 

lin x = x

func x = - exp (-x) * log x

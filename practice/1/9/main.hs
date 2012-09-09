{-
	main.hs

	Practice 1.9

	median from list

	input:
		list
	output:
		median
-}

module Main where

main:: IO ()

main = print(median [1, 1, 8, 10])

median x
	| mod (length x) 2 == 1 = x !! m
	| otherwise = ( (x !! m) + (x !! (m - 1) ) ) * 0.5
	where
		m = div (length x) 2

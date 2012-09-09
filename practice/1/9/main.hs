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

main = print(median [1, 1, 1, 8])

median x	| mod (middle x) 2 == 1 = x!!(middle x)
			| otherwise = ((x!!(middle x) + x!!((middle x)+1)) / 2)
	where
		middle x =  div (length x) 2

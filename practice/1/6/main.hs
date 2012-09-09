{-
	main.hs

	multiply two num
-}

module Main where

main = print $mul (-10) 55

mul a b	| abs a < abs b = (mult b $abs a) * sign a
		| otherwise = (mult a $abs b) * sign b
	where
		mult a b	| b == 0 = 0
					| otherwise = a + mult a (b - 1)

sign a	| a < 0 = -1
		| otherwise = 1

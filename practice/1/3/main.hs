{-
	main.hs

	Practice 9.3
	
	gcd

	input:
		x, y
	output:
		gcd(x,y)
-}

module Main where

main::IO ()

main = print (gcdAnother 10 55) >> print (gcd 10 55)

{-
	Idea:
	gcd(a,b) =	|a, b == 0
				|gcd(b, a mod b), otherwise	
-}

gcdAnother a b	| b == 0 = a
				| otherwise = gcdAnother b (mod a b)

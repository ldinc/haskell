{-
	main.hs

	Practice 1.5

	lcm
-}

{-
	Idea:
	lcm(a,b) = |ab|/gcd(a,b)
-}

module Main where

main::IO ()

main = print (lcm 10 15) >> print (lcmAnother 10 15)

gcdAnother a b	| b == 0 = a
				| otherwise = gcdAnother b (mod a b)

lcmAnother a b = div (abs a*b) (gcdAnother a b)


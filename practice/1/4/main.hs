{-
	main.hs

	Practice 1.4

	Primality test
	
	input:
		x
	output:
		True\False =)
-}

module Main where

main::IO ()

main = print (primalityTest 4)

{-
	Idea:
		тупой перебор... даже до корня оптимизацию не дали =(		
-}

primalityTest x = tmp x 2
	where
		tmp x y | y == x = True
				| mod x y == 0 = False 
				| y < x = tmp x (y + 1)

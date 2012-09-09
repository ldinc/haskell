{-
	main.hs

	The first haskell app.
	Practice 1.1
	
	input:
		a,b,c from ax^2+bx+c=0
	output:
		real roots
-}

main::IO ()

main = print (solveQE (-4) 2 6)

solveQE a b c 	| d < 0 = []
				| d == 0 = [x1]
				| otherwise = [x1, x2]
	where
		d = b^2 - 4*a*c
		x1 = ( -b + sqrt d ) / (2*a)
		x2 = ( -b - sqrt d ) / (2*a)

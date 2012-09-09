{-
	main.hs

	Practice 1.2
		Fibonacci number	
	input:
		n
	output:
		fib(n)
-}

module Main where

main::IO ()

main = do
	print ("fib(0) :")
	print (fib(0))
	print ("fib(1)")
	print (fib(1))
	print ("fib(2)")
	print (fib(2))
	print ("fib(3)")
	print (fib(3))
	print ("fib(5)")
	print (fib(5))
	print ("fib(-4)")
	print (fib(-4))
	return ()

{-
	Idea:
	fib(n) =	| fib(n+2)-fib(n+1), n < 0
				| 0, n == 0
				| 1, n == 1
				| fib(n-1)+fib(n-2), n > 1
-}

fib n	| n < 0 = fib(n + 2) - fib(n + 1)
		| n == 0 = 0
		| n == 1 = 1
		| n > 1 = fib(n - 1) + fib(n - 2)

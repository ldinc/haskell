{- Calculator, based on stack kernel. Using Reverse Polish notation. -}

module Main where

import Text.Printf
import Text.Show

main::IO ()

main = do
	printf "Calc. PPn. Ready:\n"
	str <- getLine
	print (words str)
	mainloop (words str)
	return () 

-- queue terminator
mainloop::[String] -> IO ()

mainloop queue
	| (queue!!0) == ":q" = printf "Goodbye!\n"
	| otherwise = do
		-- try to parse ppn
		printf "Result = %s\n" (parseppn queue [])
		--
		str <- getLine
		print (words str)
		mainloop (words str)

parseppn::[String] -> [Double] -> String 

parseppn [] (x:[]) = show x
parseppn ("+":qs) (x:y:xs) = parseppn qs ((x + y):xs)
parseppn ("-":qs) (y:x:xs) = parseppn qs ((x - y):xs)
parseppn ("/":qs) (y:x:xs) = parseppn qs ((x / y):xs)
parseppn ("*":qs) (x:y:xs) = parseppn qs ((x * y):xs)
parseppn (a:qs) stack = parseppn qs ((read a::Double):stack)



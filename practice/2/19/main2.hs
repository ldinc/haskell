module Main where

import Text.Printf
import Text.Show

main::IO ()

main = printf "Calc. PPn. Ready:\n" >> mainloop

mainloop = do
	str <- getLine
	if (words str)!!0 == ":q" then printf "Goodbye!\n" >> return ()
		else parseppn (words str) [] >> mainloop

parseppn::[String] -> [Double] ->IO ()                 

parseppn [] (x:[]) = printf "Result = %f\n" x
parseppn ("+":qs) (x:y:xs) = parseppn qs ((x + y):xs)
parseppn ("-":qs) (y:x:xs) = parseppn qs ((x - y):xs)
parseppn ("/":qs) (y:x:xs) = parseppn qs ((x / y):xs)
parseppn ("*":qs) (x:y:xs) = parseppn qs ((x * y):xs)
parseppn (a:qs) stack = parseppn qs ((read a::Double):stack)
 

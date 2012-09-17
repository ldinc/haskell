module Main where

import MyMath

main::IO ()

main = do
	print "Simple"
	print (locminGR func 1 2 0.0001)
	print "Bounded 10"
	print (locminGRbounded func 1 2 0.0001 10)
	print "DBG"
	locminGRex func 1 2 0.0001
	return ()

lin x = x

func x = - exp (-x) * log x

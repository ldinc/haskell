module Main where

import MyAlgorithm

main::IO ()

--main = print ()

cmp1::String -> String -> Bool

cmp1 a b
	| length a < length b = True
	| otherwise = False

cmp2::String -> String -> Bool

cmp2 "" "" = True
cmp2 "" b = True
cmp2 a "" = False
cmp2 (x:[]) b
	| x < b!!1 = True
	| otherwise = False
cmp2 a (x:[]) = cmp2 (x:[]) a
cmp2 (x:y:[]) b = cmp2 (x:[]) b
cmp2 a (x:y:[]) = cmp2 (x:[]) a
cmp2 a b = a!!1 < b!!1 && a!!3 < b!!3

main = print (qsort ["Hi", "My", "Dear", "FRIEND", "LOTS", "of", "Laught"] cmp1) >> print (qsort ["Hi", "My", "Dear", "FRIEND", "LOTS", "of", "Laught"] cmp2)

module Main where

import MyAlgorithm
import Char (toLower)

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
	| toLower x < toLower (b!!1) = True
	| otherwise = False
cmp2 a (x:[]) = cmp2 (x:[]) a
cmp2 (x:y:[]) b = cmp2 (x:[]) b
cmp2 a (x:y:[]) = cmp2 (x:[]) a
cmp2 a b = toLower (a!!1) < toLower (b!!1) && toLower (a!!3) < toLower (b!!3)

main = print (qsort ["Hi", "My", "Dear", "FRIEND", "LOTS", "of", "Laught"] cmp1) >> print (qsort ["Hi", "My", "Dear", "FRIEND", "LOTS", "of", "Laught"] cmp2)

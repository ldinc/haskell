module Main where

import MyMath

main::IO ()

main = print (range 1 10 1) >> print (rangef 1 10 0.9)

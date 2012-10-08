module Main where

import MyQuantile

main::IO ()

main =
	return ([1,1,1,3,4,5,11,23,42,65,90,100,230,321,1200,100004])
	>>= (\x -> print (getMedian x) >> return x)
	>>= (\x -> print (getQuartiles x) >> return x)
	>>= (\x -> print (getInterquartileRange x) >> return x)
	>>= (\x -> print (getWithoutOutliers x) >> return x)
	>>= (\x -> print (getOutliers x))

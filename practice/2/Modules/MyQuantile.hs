{- |
 - Module : MyQuantile.hs  <File name or $Header$ to be replaced automatically>
 - Description : Simple quantile lib for practice
 - Copyright : (c) ldinc "drogunov.igor@gmail.com"
 - License : <license>
 - -}

module MyQuantile where


-------------------------------------------------------------------------------
{- Interquartile range function -}
-------------------------------------------------------------------------------
getInterquartileRange::Fractional a => [a] -> a

getInterquartileRange x = ((getQuartiles x) !! 2) - ((getQuartiles x) !! 0)
-------------------------------------------------------------------------------

-------------------------------------------------------------------------------
{- Median function for sorted list-}
-------------------------------------------------------------------------------
getMedian::Fractional a => [a] -> a

getMedian x
	| odd (length x) = x !! m
	| otherwise = ( (x !! m) + (x!!(m - 1)) )*0.5
	where
		m = div (length x) 2
-------------------------------------------------------------------------------

-------------------------------------------------------------------------------
-------------------------------------------------------------------------------
-------------------------------------------------------------------------------

-------------------------------------------------------------------------------
{- Return outliners.
 - Outliers are observations that fall below Q1-1.5(IQR) or above Q3+1.5(IQR).-}
-------------------------------------------------------------------------------
getOutliers::(Fractional a, Ord a) => [a] -> [a]

getOutliers x = [a|a <- x, a < low] ++ [a|a <- x, a > high]
	where
		low = getQuartiles x !! 0 - 1.5 * getInterquartileRange x
		high = getQuartiles x !! 2 + 1.5 * getInterquartileRange x
-------------------------------------------------------------------------------

-------------------------------------------------------------------------------
{- Quartiles function for sorted list. Return [first, second(median), third] -}
-------------------------------------------------------------------------------
getQuartiles::Fractional a => [a] -> [a]

getQuartiles x = (getMedian l:getMedian x:getMedian r:[])
	where
		m = div (length x) 2
		l = take m x
		r = drop m x
-------------------------------------------------------------------------------

-------------------------------------------------------------------------------
{- Return list without outliers. -}
-------------------------------------------------------------------------------
getWithoutOutliers::(Fractional a, Ord a) => [a] -> [a]

getWithoutOutliers x = [a|a <- x, a >= low && a <= high]
	where
        low = getQuartiles x !! 0 - 1.5 * getInterquartileRange x
        high = getQuartiles x !! 2 + 1.5 * getInterquartileRange x
-------------------------------------------------------------------------------

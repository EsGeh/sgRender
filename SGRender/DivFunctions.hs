module SGRender.DivFunctions where

import SGRender.Block


{- |@
f :: DivBlocks t
f (indexDim, dist) [dimRel1, dimRel2, ...] =
	[dist1, dist2, ...]
	where
		dist1, dist2, ... \"describe a division of 'dist'\", such that
		sum [dist1, dist2, ...] = dist (DivBlocks-Condition)
@
-}
type DivBlocks dist = DefOneDim dist -> [DimRel dist] -> [dist] 

{- |@
f :: DivDist a
f maxDistance listDist =
	try to make the following true:
		sum listDist == maxDistance (DivDist-Condition:)
@
-}
type DivDist a = a -> [a] -> [a]


{-| still unsafe...! -}
divDistCut :: Int -> [Int] -> [Int]
divDistCut maxDist listDist = case listDist of
	[] -> []
	x:[] -> [maxDist]
	(x:xs) -> if x > maxDist
		then maxDist:( take (length xs) $ repeat 0)
		else x : (divDistCut (maxDist - x) xs)

{- |@
try to divide (maxWidth-sum listWidth) equally over listWidth
if an equal division is not possible:
	give more weight to the leftmost elements
	DivDist-Condition: ok
		(but be careful:
			result can have negative entries!
		also:
			the division is not always very fair!)
		
@

example:

@
divDistEqual 5 [1,1,1] = [2,2,1]
divDistEqual 2 [1,1,1] = [0,1,1]
@
-}
divDistEqual :: Int -> [Int] -> [Int]
divDistEqual maxWidth listWidth = zipWith (+) (divDiff diff (length listWidth)) listWidth
	where
		diff = maxWidth - sum listWidth

-- |divide 'diff' into 'count' pieces 'res', so that t sum res == 'diff'
-- example: divDiff 5 3 = [2, 2, 1]
-- example: divDiff -5 3 = [-2, -2, -1]
divDiff :: Int -> Int -> [Int]
divDiff diff count = case count of
	0 -> case diff of {0 -> []; _ -> error "divError" }
	_ -> let oneElem = fromIntegral diff / fromIntegral count in
		(ceiling oneElem) : divDiff (diff - ceiling oneElem) (count-1)

{-
{-|example:
-}
divB :: Num dist => DivBlocks dist -- DefOneDim Int -> [DimRel Int] -> [Int]
divB defOneDim listDimRel = listDimRel <*> [(1-fst defOneDim, 1)]
-}

{- |give any DimRel the maximum distance in direction.
attention this doesn't fulfill the DivDist-Condition!

example:

@
divB (x, ?) [dimRel1, dimRel2, ...] = [dimRel1 (y,1), dimRel2 (y,1), ...]
	= [maxDist x dimRel1, maxDist x dimRel2, ...]
@
-}
divToMaxDist :: Num dist => DivBlocks dist
divToMaxDist defOneDim listDimRel = map (maxDist (fst defOneDim)) listDimRel


{- |give any DimRel the minimum distance in direction x

example:

@
divB (x, ?) [dimRel1, dimRel2, ...] = [minDist x dimRel1, minDist x dimRel2, ...]
@
-}
divToMinDist :: Num dist => DivBlocks dist
divToMinDist defOneDim listDimRel = map (minDist (fst defOneDim)) listDimRel

-- |calculates the maximum distance in the 'indexDim' direction
maxDist indexDim dimRel = dimRel ((1-indexDim), 1)

-- |calculates the minimum distance in the 'indexDim' direction
minDist indexDim dimRel = dimRel (1-indexDim, maxDist (1-indexDim) dimRel)

{-|example:

@
divBlocks (x,5) [dimRel1, dimRel2, ...] = divDistEqual 5 [dimRel1 (y,1), dimRel2 (y,1), ...]
@

This means:

@
	divBlocks (x, dist) [dimRel1, dimRel2, ...] =
@

-}
divBlocks :: DivBlocks Int -- DefOneDim Int -> [DimRel Int] -> [Int]
divBlocks defOneDim listDimRel =
	let maxDistDivision = divToMaxDist defOneDim listDimRel
	in
		divDistEqual (snd defOneDim) $ maxDistDivision 

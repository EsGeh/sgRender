{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}
module SGRender.Block where

import SGRender.Render

import SGData.Vector2D
import Card.Card
import Card.Unary
import SGData.Matrix hiding(Width,Height)
import Data.Maybe
import Data.Monoid
import Control.Applicative
import Debug.Trace

data Block a = Block {
	runBlock :: Matrix a
}
instance MultiMonoid (Block a) N2 where
	mmappend indexDim l r = case toInt indexDim of
		0 -> horiBlockComb l r
		1 -> vertBlockComb l r
		_ -> error "mappend defined in two dimensions only!"

instance Show (Block Char) where
	show block =  showMatrix (runBlock block)

showMatrix matr = unlines $ [mGetRow iRow matr | iRow <- mGetAllIndexRow matr]
{-
instance (Show a) => Show (Block a) where
	show block = show $ (runBlock block)
-}

horiBlockComb :: ReprComb (Block a)
horiBlockComb l@(Block matrL) r@(Block matrR) = if (vecX $ mGetSize $ matrL) /= (vecX $ mGetSize $ matrR) && not ((mGetSize matrR)==(0,0) || (mGetSize matrR)==(0,0) )
	then error "horiBlock Exception"
	else Block $ m (vecX $ mGetSize matrL, (vecY $ mGetSize matrL) + (vecY $ mGetSize matrR)) valFromIndex
	where
		valFromIndex index@(row,col) = if col < vecY (mGetSize matrL)
			then mGet index matrL
			else mGet (index |-| (0, vecY (mGetSize matrL))) matrR

vertBlockComb :: ReprComb (Block a)
vertBlockComb l@(Block matrL) r@(Block matrR) = if (vecY $ mGetSize $ matrL) /= (vecY $ mGetSize $ matrR) && not ((mGetSize matrR)==(0,0) || (mGetSize matrR)==(0,0) )
	then error "vertBlock Exception"
	else Block $ m ((vecX $ mGetSize matrL) + (vecX $ mGetSize matrR), vecY $ mGetSize matrL) valFromIndex
	where
		valFromIndex index@(row,col) = if row < vecX (mGetSize matrL)
			then mGet index matrL
			else mGet (index |-| (vecX (mGetSize matrL),0)) matrR

data RenderToBlockParams src char = RenderToBlockParams {
	showF :: (src -> [char]),
	fillTile :: [char],
	parenthesis :: [char]
}
renderToBlockParamsStd :: (Show a) => RenderToBlockParams a Char
renderToBlockParamsStd = RenderToBlockParams {
	showF = show,
	fillTile = " ",
	parenthesis = ".."
}

type IndexDim = Int
type DefOneDim value = (IndexDim, value)
type DimRel value = (IndexDim, value) -> value -- IndexDim -> value -> value

sizeFromValue defOneDim dimRel = let (x:y:_) = insertAt (fst defOneDim) (snd defOneDim) [dimRel defOneDim] in
	(x,y)

--bla :: DefOneDim value -> Size Int -> 

renderToBlock :: Show src => RenderToBlockParams src char -> RenderMethod
	src
	(Block char)
	(Size Int) -- params
	(DimRel Int) -- srcInfo
renderToBlock params = renderToBlock'
	srcToDimRel
	(showF params)
	(fillTile params)
	where
		srcToDimRel src (indexDim,value) = ceiling $ fromIntegral (length $ show src) / fromIntegral value

horizontal listRenderMeth = combine
	concatInfo
	horiBlockComb
	calcSubParams
	listRenderMeth
	where
		calcSubParams :: Size Int -> [DimRel Int] -> [Size Int]
		calcSubParams size listDimRel = zip
			(divF (vecX size) $ (zipWith (\f x -> f x) listDimRel (repeat $ (1,vecY size))))
			(repeat $ vecY size)

		--calcSubParams :: DefOneDim Int -> [DimRel Int] -> [Size Int]
		{-
		calcSubParams defOneDim listDimRel = repeat $ 
			foldl (\l r -> (max (vecX l) (vecX r), max (vecY l) (vecY r))) (0,0) $
				map (sizeFromValue defOneDim) listDimRel
		-}
		concatInfo :: [DimRel Int] -> DimRel Int 
		concatInfo listDimRel defOneDim = (case fst defOneDim of {0 -> snd; 1 -> fst}) $ foldl
			(\l r -> ((vecX l) + (vecX r), max (vecY l) (vecY r) ))
			(0,0) 
			((\a -> trace (show a) a) $ zipWith (\f p -> f p) (map sizeFromValue listDivOneDim) listDimRel)
			where
				listDivOneDim = (\a -> trace (show a) a) $ case fst defOneDim of
					1 -> take (length listDimRel) $ repeat defOneDim
					0 -> zip
						(repeat 0)
						(divDiff (snd defOneDim) (length listDimRel))
		{-
		concatInfo listDimRel defOneDim = (case fst defOneDim of {0 -> snd; 1 -> fst}) $ foldl
			(\l r -> ((vecX l) + (vecX r), max (vecY l) (vecY r) ))
			(0,0) 
			(map (sizeFromValue defOneDim) listDimRel)
		-}

vertical listRenderMeth = combine
	concatInfo
	vertBlockComb
	calcSubParams
	listRenderMeth
	where
		calcSubParams :: Size Int -> [DimRel Int] -> [Size Int]
		calcSubParams size listDimRel = zip
			(repeat $ vecX size)
			(divF (vecY size) $ (zipWith (\f x -> f x) listDimRel (repeat $ (0,vecX size))))

		concatInfo :: [DimRel Int] -> DimRel Int 
		concatInfo listDimRel defOneDim = (case fst defOneDim of {0 -> snd; 1 -> fst}) $ foldl
			(\l r -> (max (vecX l) (vecX r), (vecY l) + (vecY r) ))
			(0,0) 
			(map (sizeFromValue defOneDim) listDimRel)


testHori = (renderF testHori') (13,1) [1..11]
testHori' = horizontal $ repeat (renderToBlock renderToBlockParamsStd)

divF :: Int -> [Int] -> [Int]
divF maxWidth listWidth = zipWith (+) (divDiff diff (length listWidth)) listWidth
	where
		diff = maxWidth - sum listWidth

divDiff :: Int -> Int -> [Int]
divDiff diff count = case count of
	0 -> case diff of {0 -> []; _ -> error "divError" }
	_ -> let oneElem = fromIntegral diff / fromIntegral count in
		(ceiling oneElem) : divDiff (diff - ceiling oneElem) (count-1)

{-
vertical listRenderMeth = combine
	concatInfo
	vertBlockComb
	calcSubParams
	listRenderMeth
	where
		--calcSubParams :: Size Int -> [DimRel Int] -> [Size Int]
		calcSubParams size _ = repeat size
		{-
		calcSubParams :: DefOneDim Int -> [DimRel Int] -> [Size Int]
		calcSubParams defOneDim listDimRel = repeat $ 
			foldl (\l r -> (max (vecX l) (vecX r), max (vecY l) (vecY r))) (0,0) $
				map (sizeFromValue defOneDim) listDimRel
		-}
		concatInfo :: [DimRel Int] -> DimRel Int 
		concatInfo listDimRel defOneDim = (case fst defOneDim of {0 -> snd; 1 -> fst}) $ foldl
			(\l r -> (max (vecX l) (vecX r), (vecY l) + (vecY r) ))
			(0,0) 
			(map (sizeFromValue defOneDim) listDimRel)
-}

test :: Show src => RenderMethod [[src]] (Block Char) (DefOneDim Int) (DimRel Int)
test = vertical $ repeat $
	horizontal $ repeat $
		renderToBlock renderToBlockParamsStd

--renderToBlock infoFromSrc params = renderToBlock' infoFromSrc (showF params) (fillTile params)

renderToBlock' :: (src -> srcInfo) -> (src -> [char]) -> [char] -> RenderMethod src (Block char) (Size Int) srcInfo 
renderToBlock' infoFromSrc showF fillTile = RenderMeth {
	renderF = newRenderF,
	srcInfo = infoFromSrc
} where
	newRenderF size src = Block $ fromJust $ mFromListRow $ chop (vecX size) $ take area $ showF src ++ cycle fillTile where
		area = vecX size * vecY size

type Width = Int
type Height = Int
type MinWidth = Width
type MinHeight = Height
type Area = Int

chop width list = case list of
	[] -> [] 
	_ -> take width list : chop width (drop width list)

insertAt index val list = take index list ++ [val] ++ drop index list

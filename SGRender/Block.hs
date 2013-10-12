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

import SGData.Tree

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
horiBlockComb l@(Block matrL) r@(Block matrR) = if (vecX $ mGetSize $ matrL) /= (vecX $ mGetSize $ matrR) && not ((mGetSize matrL)==(0,0) || (mGetSize matrR)==(0,0) )
	then error "horiBlock Exception"
	else Block $ m (vecX $ mGetSize matrL, (vecY $ mGetSize matrL) + (vecY $ mGetSize matrR)) valFromIndex
	where
		valFromIndex index@(row,col) = if col < vecY (mGetSize matrL)
			then mGet index matrL
			else mGet (index |-| (0, vecY (mGetSize matrL))) matrR

vertBlockComb :: ReprComb (Block a)
vertBlockComb l@(Block matrL) r@(Block matrR) = if (vecY $ mGetSize $ matrL) /= (vecY $ mGetSize $ matrR) && not ((mGetSize matrL)==(0,0) || (mGetSize matrR)==(0,0) )
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

sizeFromValue dimRel defOneDim = let (x:y:_) = insertAt (fst defOneDim) (snd defOneDim) [dimRel defOneDim] in
	(x,y)

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

horizontal calcSubParams concatInfo listRenderMeth = combine
	concatInfo
	horiBlockComb
	calcSubParams
	listRenderMeth

vertical calcSubParams concatInfo listRenderMeth = combine
	concatInfo
	vertBlockComb
	calcSubParams
	listRenderMeth


renderListVerticalFromHeight :: Show src => DivDist Int -> RenderMethod [src] (Block Char) Height MinHeight
renderListVerticalFromHeight divDist = vertical calcSizeVert calcInfo $
	repeat $ renderToBlock renderToBlockParamsStd
	where
		calcSizeVert :: Height -> [DimRel Int] -> [Size Int]
		calcSizeVert height listDimRel = zip
			(take (length listDimRel) $ repeat $ maximum $ zipWith (\f s -> f s) listDimRel (repeat (1,1)))
			(divDist height $ take (length listDimRel) $ repeat 1)
			--(take (length listDimRel -1) (repeat 1) ++ [ height - (length listDimRel -1)])
		calcInfo listDimRel = length listDimRel

renderListVerticalFromWidth :: Show src => DivDist Int -> RenderMethod [src] (Block Char) Width MinWidth
renderListVerticalFromWidth divDist = vertical calcSizeVert calcInfo $
	repeat $ renderToBlock renderToBlockParamsStd
	where
		calcSizeVert :: Width -> [DimRel Int] -> [Size Int]
		calcSizeVert width listDimRel = zip
			(repeat width)
			(take (length listDimRel) $ repeat 1)
		calcInfo listDimRel = maximum $ listDimRel <*> [(1,1),(1,1)]

renderListHorizontalFromWidth :: Show src => DivDist Int -> RenderMethod [src] (Block Char) Width MinWidth
renderListHorizontalFromWidth divDist = horizontal calcSizeVert calcInfo $
	repeat $ renderToBlock renderToBlockParamsStd
	where
		calcSizeVert :: Width -> [DimRel Int] -> [Size Int]
		calcSizeVert width listDimRel = (\a -> trace (show a) a) $ zip
			(divDist width $ zipWith (\f s -> f s) listDimRel (repeat (1,1)))
			(repeat 1)
		calcInfo listDimRel = sum (zipWith (\f s -> f s) listDimRel (repeat (1,1)))

test :: Show src => RenderMethod [[src]] (Block Char) () ()
test = horizontal calcSubParamsHori calcInfoHori $ repeat $
	renderListVerticalFromHeight divDontDeform
	where
		calcSubParamsHori _ listHeight = repeat $ maximum listHeight
		calcInfoHori _ = ()

test2 :: Show src => RenderMethod [[src]] (Block Char) () ()
test2 = vertical calcSubParamsHori calcInfoHori $ repeat $
	renderListHorizontalFromWidth divDontDeform
	where
		calcSubParamsHori _ listWidth = repeat $ maximum listWidth
		calcInfoHori _ = ()

testTree = node 1 $ [ leaf 1.1, leaf 1.2 ]

renderTree :: Show src => RenderMethod (Tree src) (Block Char) Width Width
renderTree = RenderMeth {
	renderF = newRenderF,
	srcInfo = newSrcInfo
}
	where
		newRenderF width src = (renderF $ renderHeadAndSubTrees) width $
			(value src, children src)
		newSrcInfo tree = (srcInfo renderHeadAndSubTrees) (value tree, children tree)

renderHeadAndSubTrees :: Show src => RenderMethod (src,[Tree src]) (Block Char) Width Width
renderHeadAndSubTrees = combine2
	combineSrcInfo
	vertBlockComb
	calcSubParams
	(renderToBlock renderToBlockParamsStd)
	renderSubTrees
	where
		combineSrcInfo dimRelU widthD = max (dimRelU (1,1)) widthD --(dimRel (1,1)) (dimRelR (1,1))

		calcSubParams :: Width -> (DimRel Int, Width) -> (Size Int, Width)
		calcSubParams widthAllInAll (dimRelU,widthD) = ((widthAllInAll,1),widthAllInAll) -- ((sizeFromValue dimRel) (1,1), (sizeFromValue dimRelR) (1,1))

renderSubTrees :: Show src => RenderMethod [Tree src] (Block Char) Width Width
renderSubTrees = horizontal
	calcSubParams
	(\listWidth -> maximum listWidth) 
	(repeat renderTree)
	where
		calcSubParams width listWidth = divEqual width listWidth


type DivDist a = a -> [a] -> [a]

divDontDeform :: Int -> [Int] -> [Int]
divDontDeform maxDist listDist = case listDist of
	[] -> []
	x:[] -> [maxDist]
	(x:xs) -> if x > maxDist
		then maxDist:( take (length xs) $ repeat 0)
		else x : (divDontDeform (maxDist - x) xs)

divEqual :: Int -> [Int] -> [Int]
divEqual maxWidth listWidth = zipWith (+) (divDiff diff (length listWidth)) listWidth
	where
		diff = maxWidth - sum listWidth

divDiff :: Int -> Int -> [Int]
divDiff diff count = case count of
	0 -> case diff of {0 -> []; _ -> error "divError" }
	_ -> let oneElem = fromIntegral diff / fromIntegral count in
		(ceiling oneElem) : divDiff (diff - ceiling oneElem) (count-1)


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

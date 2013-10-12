{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}
module SGRender.Block(
	-- * basic types
	Block(),
	-- * basic renderMethods
	renderToBlock, renderToBlockParamsStd, RenderToBlockParams(..),
	filledBlock,
	-- * combinators
	horizontal, vertical,
	-- * rendering from lists
	renderListHorizontalFromWidth,
	renderListVerticalFromWidth, renderListVerticalFromHeight,
	-- * rendering a tree
	renderTree,
) where

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

horiBlockComb :: ReprComb (Block a)
horiBlockComb l@(Block matrL) r@(Block matrR) = if (mGetSize matrL)==(0,0) 
	then (Block matrR)
	else if ((mGetSize matrR) == (0,0)) then (Block matrL)
		else if (vecX $ mGetSize $ matrL) /= (vecX $ mGetSize $ matrR) 
			then error "horiBlock Exception"
			else Block $ m (vecX $ mGetSize matrL, (vecY $ mGetSize matrL) + (vecY $ mGetSize matrR)) valFromIndex
			where
				valFromIndex index@(row,col) = if col < vecY (mGetSize matrL)
					then mGet index matrL
					else mGet (index |-| (0, vecY (mGetSize matrL))) matrR

vertBlockComb :: ReprComb (Block a)
vertBlockComb l@(Block matrL) r@(Block matrR) = if (mGetSize matrL)==(0,0) 
	then (Block matrR)
	else if ((mGetSize matrR) == (0,0)) then (Block matrL)
		else if (vecY $ mGetSize $ matrL) /= (vecY $ mGetSize $ matrR) 
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

horizontal fillEmpty calcSubParams concatInfo listRenderMeth = combine
	horiBlockComb
	fillEmpty
	calcSubParams
	concatInfo
	listRenderMeth

vertical fillEmpty calcSubParams concatInfo listRenderMeth = combine
	vertBlockComb
	fillEmpty
	calcSubParams
	concatInfo
	listRenderMeth

filledBlock fillTile size = Block $ fromJust $ mFromListRow $ take (vecY size) $ repeat $
	take (vecX size) $ cycle fillTile
zeroBlock = Block $ m (0,0) (const 'x')

renderListVerticalFromHeight :: Show src => DivDist Int -> RenderMethod [src] (Block Char) Height MinHeight
renderListVerticalFromHeight divDist = vertical (\height -> filledBlock " " (1,height)) calcSizeVert calcInfo $
	repeat $ renderToBlock renderToBlockParamsStd
	where
		calcSizeVert :: Height -> [DimRel Int] -> [Size Int]
		calcSizeVert height listDimRel = zip
			(take (length listDimRel) $ repeat $ maximum $ zipWith (\f s -> f s) listDimRel (repeat (1,1)))
			(divDist height $ take (length listDimRel) $ repeat 1)
			--(take (length listDimRel -1) (repeat 1) ++ [ height - (length listDimRel -1)])
		calcInfo listDimRel = length listDimRel

renderListVerticalFromWidth :: Show src => DivDist Int -> RenderMethod [src] (Block Char) Width MinWidth
renderListVerticalFromWidth divDist = vertical (\width -> filledBlock " " (width,1)) calcSizeVert calcInfo $
	repeat $ renderToBlock renderToBlockParamsStd
	where
		calcSizeVert :: Width -> [DimRel Int] -> [Size Int]
		calcSizeVert width listDimRel = zip
			(repeat width)
			(take (length listDimRel) $ repeat 1)
		calcInfo listDimRel = maximum $ listDimRel <*> [(1,1),(1,1)]

renderListHorizontalFromWidth :: Show src => DivDist Int -> RenderMethod [src] (Block Char) Width MinWidth
renderListHorizontalFromWidth divDist = horizontal (\width -> filledBlock " " (width,1)) calcSizeVert calcInfo $
	repeat $ renderToBlock renderToBlockParamsStd
	where
		calcSizeVert :: Width -> [DimRel Int] -> [Size Int]
		calcSizeVert width listDimRel = (\a -> trace (show a) a) $ zip
			(divDist width $ zipWith (\f s -> f s) listDimRel (repeat (1,1)))
			(repeat 1)
		calcInfo listDimRel = sum (zipWith (\f s -> f s) listDimRel (repeat (1,1)))

test :: Show src => RenderMethod [[src]] (Block Char) () ()
test = horizontal (\_ -> zeroBlock) calcSubParamsHori calcInfoHori $ repeat $
	renderListVerticalFromHeight divDontDeform
	where
		calcSubParamsHori _ listHeight = repeat $ maximum listHeight
		calcInfoHori _ = ()

test2 :: Show src => RenderMethod [[src]] (Block Char) () ()
test2 = vertical (const zeroBlock) calcSubParamsHori calcInfoHori $ repeat $
	renderListHorizontalFromWidth divDontDeform
	where
		calcSubParamsHori _ listWidth = repeat $ maximum listWidth
		calcInfoHori _ = ()

testTree = node 1 $ [ leaf 1.1, leaf 1.2, node 1.3 [ leaf 2, leaf 3] ]

renderTree :: Show src => RenderMethod (Tree src) (Block Char) (Size Int) (Size Int)
renderTree = RenderMeth {
	renderF = newRenderF,
	srcInfo = newSrcInfo
}
	where
		newRenderF size src = (renderF $ renderHeadAndSubTrees) size $
			(value src, children src)
		newSrcInfo tree = (srcInfo renderHeadAndSubTrees) (value tree, children tree)

renderHeadAndSubTrees :: Show src => RenderMethod (src,[Tree src]) (Block Char) (Size Int) (Size Int)
renderHeadAndSubTrees = combine2
	vertBlockComb
	calcSubParams
	combineSrcInfo
	(renderToBlock renderToBlockParamsStd)
	renderSubTrees
	where
		combineSrcInfo dimRelHead sizeChildren = (
			max (dimRelHead (1,1)) (vecX sizeChildren),
			1 + vecY sizeChildren)
		--max (dimRelU (1,1)) widthD 

		calcSubParams :: Size Int -> (DimRel Int, Size Int) -> (Size Int, Size Int)
		calcSubParams wholeSize (dimRelU,sizeD) = (
			(width, heightHead),
			(width, heightChildren))
			where
				width = vecX wholeSize
				heightHead : heightChildren : _ = divDontDeform (vecY wholeSize)
					[1, vecY sizeD]
				--((widthAllInAll,1),widthAllInAll) 

renderSubTrees :: Show src => RenderMethod [Tree src] (Block Char) (Size Int) (Size Int)
renderSubTrees = horizontal (\size -> filledBlock "+" size)
	calcSubParams
	(\listSize -> case listSize of
		[] -> (0,0)
		_ -> foldl1 (\l r -> ((vecX l) + (vecX r), max (vecY l) (vecY r))) $
			listSize)
			--(maximum listWidth,length listWidth)) 
	(repeat renderTree)
	where
		calcSubParams size listSize = zip
			(divEqual (vecX size) (map vecX listSize))
			(repeat $ vecY size)

renderMinimal renderMeth src = case renderMeth of 
	RenderMeth{ renderF=renderF, srcInfo=srcInfo} -> renderF (srcInfo src) src

testRenderTree = (renderF $ renderTree) (20,20) testTree


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

type Size a = Vec a

chop width list = case list of
	[] -> [] 
	_ -> take width list : chop width (drop width list)

insertAt index val list = take index list ++ [val] ++ drop index list

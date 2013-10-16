{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}
module SGRender.Block (
	-- * basic types
	Block(),
	--horiBlockComb, vertBlockComb,
	-- * basic renderMethods
	renderToBlock, renderToBlockParamsStd, RenderToBlockParams(..),
	-- * FillFunctions
	filledBlock,
	-- * divDist Functions
	divDistEqual, divDistCut,
	-- * little type aliases
	Size,
	Width, Height,
	DimRel, DefOneDim,
	sizeFromDimRel,
) where 

import SGRender.Render

import SGData.Vector2D
import SGCard.Card
import SGCard.Unary
import SGData.Matrix hiding(Width,Height)
import Data.Maybe
import Data.Monoid
import Control.Applicative

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

sizeFromDimRel :: DimRel val -> (DefOneDim val -> Size val)
sizeFromDimRel dimRel defOneDim = let (x:y:_) = insertAt (fst defOneDim) (snd defOneDim) [dimRel defOneDim] in
	(x,y)

renderToBlock :: Show src => RenderToBlockParams src char -> RenderMethod
	src
	(Block char)
	(Size Int) -- params
	(DimRel Int) -- srcInfo
renderToBlock params = renderToBlock'
	(showF params)
	srcToDimRel
	(fillTile params)
	where
		srcToDimRel src (indexDim,value) = ceiling $ fromIntegral (length $ show src) / fromIntegral value




filledBlock fillTile size = Block $ fromJust $ mFromListRow $ take (vecY size) $ repeat $
	take (vecX size) $ cycle fillTile

zeroBlock = Block $ m (0,0) (const 'x')


type DivDist a = a -> [a] -> [a]

divDistCut :: Int -> [Int] -> [Int]
divDistCut maxDist listDist = case listDist of
	[] -> []
	x:[] -> [maxDist]
	(x:xs) -> if x > maxDist
		then maxDist:( take (length xs) $ repeat 0)
		else x : (divDistCut (maxDist - x) xs)

divDistEqual :: Int -> [Int] -> [Int]
divDistEqual maxWidth listWidth = zipWith (+) (divDiff diff (length listWidth)) listWidth
	where
		diff = maxWidth - sum listWidth

divDiff :: Int -> Int -> [Int]
divDiff diff count = case count of
	0 -> case diff of {0 -> []; _ -> error "divError" }
	_ -> let oneElem = fromIntegral diff / fromIntegral count in
		(ceiling oneElem) : divDiff (diff - ceiling oneElem) (count-1)


renderToBlock' :: (src -> [char]) -> (src -> srcInfo) -> [char] -> RenderMethod src (Block char) (Size Int) srcInfo 
renderToBlock' showF infoFromSrc fillTile = renderMeth newRenderF infoFromSrc
	where
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

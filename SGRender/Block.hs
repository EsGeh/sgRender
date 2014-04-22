{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}
{-|This module defines some data type to represent text that fits into a rectangle ('Block') and basic RenderMethods to render from some src type into it.

The general idea is to specialize a RenderMethod src repr params srcInfo like so:

	- repr = 'Block' char

	- params = 'Size' Int

	- srcInfo = 'DimRel' Int

'renderToBlock' implements a basic 'RenderMethod' of that kind.

-}
module SGRender.Block (
	-- * basic types
	Block(),
	--horiBlockComb, vertBlockComb,
	-- * basic renderMethods
	renderToBlock, renderToBlockParamsStd, RenderToBlockParams(..),
	-- * FillFunctions
	filledBlock,
	-- * working with sizes
	-- ** little type aliases
	Size,
	Width, Height,
	DimRel, DefOneDim,
	IndexDim,
	-- ** functions to work with sizes
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
{- |
@
renderToBlockParamsStd = RenderToBlockParams {
	showF = show,
	fillTile = \" \",
	parenthesis = \"..\"
}
@
-}
renderToBlockParamsStd :: (Show a) => RenderToBlockParams a Char
renderToBlockParamsStd = RenderToBlockParams {
	showF = show,
	fillTile = " ",
	parenthesis = ".."
}

-- | 0 for x-Axis, 1 for y-Axis, ...
type IndexDim = Int
type DefOneDim value = (IndexDim, value)
-- | given one side of a rectangle, return the other
type DimRel value = (IndexDim, value) -> value -- IndexDim -> value -> value

{- |sizeFromDimRel :: (DimRel Int) -> (IndexDim, Int) -> Size Int

given one side of a rectangle, and a 'DimRel' Int, the rectangle is defined
-}
sizeFromDimRel :: DimRel val -> (DefOneDim val -> Size val)
sizeFromDimRel dimRel defOneDim = let (x:y:_) = insertAt (fst defOneDim) (snd defOneDim) [dimRel defOneDim] in
	(x,y)

{- | Pseudo-Code:

@
given 'RenderToBlockParams'{
	showF :: src -> [char]
	fillTile :: [char]
	parenthesis :: [char]
}
returns a 'RenderMethod' {
	renderF :: Size Int -> src -> Block char
	srcInfo :: src -> srcInfo (= src -> DimRel Int)
		= src -> DefOneDim -> Int
}
where
	renderF is defined as follows (Pseudo-Code):
		1. longString = showF src ++ cycle fillTile :: [char]
		2. lines = chop longString into lines :: [[char]]
		3. return a "Block" of lines
--
	-- PRECOND: distance >= 1
	-- OTHERWISE:
	-- 	return srcInfo src (indexDim,1)
	srcInfo src (indexDim, distance) = (Pseudo-Code):
		minimumArea = length $ showF src
		minimumArea / distance
@
-}
renderToBlock :: RenderToBlockParams src char -> RenderMethod
	src
	(Block char)
	(Size Int) -- params
	(DimRel Int) -- srcInfo
renderToBlock params = renderToBlock'
	(showF params)		--showF
	srcToDimRel		--infoFromSrc:: src -> (IndexDim, Int) -> Int
	(fillTile params)	--fillTile
	where
		srcToDimRel src (indexDim,value) =
			if value <= 1 then
				length $ (showF params) src
			else
				ceiling $ fromIntegral (length $ (showF params) src) / fromIntegral value


filledBlock fillTile size = Block $ fromJust $ mFromListRow $ take (vecY size) $ repeat $
	take (vecX size) $ cycle fillTile

zeroBlock = Block $ m (0,0) (const 'x')



{- | given a
 - 	showF :: src -> [char]
 - 	infoFromSrc :: src -> srcInfo
 -	fillTile :: [char]
 - returns a 'RenderMethod'
 - 	renderF :: Size Int -> src -> Block char
 - 	srcInfo :: src -> srcInfo
 - where renderF is defined as follows (Pseudo-Code):
 - 	1. longString = showF src ++ cycle fillTile :: [char]
 - 	2. lines = chop longString into lines :: [[char]]
 - 	3. return a "Block" of lines
-}
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

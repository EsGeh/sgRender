{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}
module SGRender.Block where

import SGRender.Render

import Util.Vector2D
import Util.Card.Card
import Util.Card.Unary
import Math.Matrix
import Data.Maybe
import Data.Monoid

data Block a = Block {
	runBlock :: Matrix a
}
instance MultiMonoid (Block a) N2 where
	mmappend indexDim l r = case toInt indexDim of
		0 -> horiBlockComb l r
		1 -> vertBlockComb l r
		_ -> error "mappend defined in two dimensions only!"

{-
newtype HoriBlock a = HoriBlock { fromHoriBlock :: Block a }
newtype VertBlock a = VertBlock { fromVertBlock :: Block a }
horiBlock = HoriBlock . Block
vertBlock = VertBlock . Block
instance Monoid (HoriBlock a) where
	mempty = horiBlock $ fromJust $ mFromListRow []
	mappend l r = HoriBlock $ horiBlockComb (fromHoriBlock l) (fromHoriBlock r)
instance Monoid (VertBlock a) where
	mempty = vertBlock $ fromJust $ mFromListRow []
	mappend l r = VertBlock $ vertBlockComb (fromVertBlock l) (fromVertBlock r)
-}
instance Show (Block Char) where
	show block = unlines $ [mGetRow iRow (runBlock block) | iRow <- mGetAllIndexRow (runBlock block)]
{-
instance (Show a) => Show (Block a) where
	show block = show $ (runBlock block)
-}

horiBlockComb :: ReprComb (Block a)
horiBlockComb l@(Block matrL) r@(Block matrR) = if (vecX $ mGetSize $ matrL) /= (vecX $ mGetSize $ matrR)
	then error "horiBlock Exception"
	else Block $ m (vecX $ mGetSize matrL, (vecY $ mGetSize matrL) + (vecY $ mGetSize matrR)) valFromIndex
	where
		valFromIndex index@(row,col) = if col < vecY (mGetSize matrL)
			then mGet index matrL
			else mGet (index |-| (0, vecY (mGetSize matrL))) matrR

vertBlockComb :: ReprComb (Block a)
vertBlockComb l@(Block matrL) r@(Block matrR) = if (vecY $ mGetSize $ matrL) /= (vecY $ mGetSize $ matrR)
	then error "vertBlock Exception"
	else Block $ m ((vecX $ mGetSize matrL) + (vecX $ mGetSize matrR), vecY $ mGetSize matrL) valFromIndex
	where
		valFromIndex index@(row,col) = if row < vecX (mGetSize matrL)
			then mGet index matrL
			else mGet (index |-| (vecX (mGetSize matrL),0)) matrR

renderToBlock :: (src -> [repr]) -> [repr] -> RenderFunctionWithSize src (Block repr) (Size Int) Int
renderToBlock show fillTile = RenderFunctionWithSize {
	runRenderFWithSize = renderF,
	checkSize = checkSize
} where
	renderF size list = Block $ fromJust $ mFromListRow $ chop (vecX size) $ take area $ show list ++ cycle fillTile
		where
			area = vecX size * vecY size
	checkSize list = length $ show list

{-
renderToHoriBlock fillTile size = HoriBlock . renderToBlock fillTile size
renderToVertBlock fillTile size = VertBlock . renderToBlock fillTile size
-}

{-
stretchMethBlock show fillTile size block@(Block matr) = renderToBlock show fillTile size $ concat [ mGetRow iRow matr | iRow <- mGetAllIndexRow matr ]
-}

testMethod :: (Show src) => RenderFunctionWithSize (src,src) (Block Char) (Size Int) Int
testMethod = combineWithSize2' horiBlockComb (divHoriFromDivDist divE) (renderToBlock show ".") (renderToBlock show "*")
testMethod2 :: (Show src) => RenderFunctionWithSize (src,src) (Block Char) (Size Int) Int
testMethod2 = combineWithSize2' vertBlockComb (divHoriFromDivDist divE) (renderToBlock show ".") (renderToBlock show "*")

--testMeth :: (Card n) => n -> RenderFunctionWithSize (src,src) (Block Char) (Size Int)
testMeth indexDim = combineWithSize2 indexDim (divHoriFromDivDist divE) (renderToBlock id ".") (renderToBlock id "*")

test = (runRenderFWithSize $ testMeth n0) (10,10) ("left","right")

--testMethod2 = combWithDist (divVertFromDivDist divE) (renderToVertBlock ".") (renderToVertBlock "*")

divE count width = [4,4]


type DivSize dist = DivDist (Size dist)
divHoriFromDivDist :: DivDist dist ->  DivSize dist
divHoriFromDivDist divDist count size = zip
	(repeat $ vecY size)
	(divDist count $ vecX size)
divVertFromDivDist :: DivDist dist ->  DivSize dist
divVertFromDivDist divDist count size = zip
	(divDist count $ vecY size)
	(repeat $ vecX size)

chop width list = case list of
	[] -> [] 
	_ -> take width list : chop width (drop width list)

{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}
module SGRender.Block where

import SGRender.Render

import SGData.Vector2D
import Card.Card
import Card.Unary
import SGData.Matrix
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

data RenderWithSizeParams src char = RenderWithSizeParams {
	showF :: (src -> [char]),
	fillTile :: [char],
	parenthesis :: [char]
}

renderToBlock :: (src -> srcInfo) -> (src -> [char]) -> [char] -> RenderMethod src (Block char) srcInfo (Size Int)
renderToBlock infoFromSrc showF fillTile = RenderMeth {
	renderF = newRenderF,
	srcInfo = infoFromSrc
} where
	newRenderF size src = Block $ fromJust $ mFromListRow $ chop (vecX size) $ take area $ showF src ++ cycle fillTile where
		area = vecX size * vecY size
	
type MinArea = Int
horiWithSize :: (Show src) => Int -> RenderMethod [src] (Block Char) MinArea (Size Int)
horiWithSize height = combine
	(+)
	horiBlockComb
	listSizeFromSrcInfo
	(repeat $ renderToBlock infoFromSrc show "+")
	where
		--infoFromSrc :: src -> MinArea
		infoFromSrc src = length $ show src
		listSizeFromSrcInfo :: Size Int -> [MinArea] -> [Size Int]
		listSizeFromSrcInfo size listSrcInfo = zip
			(repeat $ maximum $ map (`div` height) listSrcInfo)
			(repeat height)
{-
rndrWithSizePStd :: (Show a) => RenderWithSizeParams a Char
rndrWithSizePStd = RenderWithSizeParams {
	showF = show,
	fillTile = " ",
	parenthesis = ".."
}


renderToBlockWithSize :: RenderWithSizeParams src char -> RenderMethodWithSize src (Block char) (Size Int) Int
renderToBlockWithSize params =
	RenderMethodWithSize {
		runRenderFWithSize = renderF,
		checkSize = checkSize
	} where
		renderF size list = Block $ fromJust $ mFromListRow $ chop (vecX size) $ take area $ show list ++ cycle fillTile'
			where
				area = vecX size * vecY size
		checkSize list = length $ show list
		show = showF params
		fillTile' = fillTile params
		parenthesis' = parenthesis params
-}

{-
vert2WithSize :: (Show src) => RenderMethodWithSize (src,src) (Block Char) (Size Int) Int
vert2WithSize = combineWithSize2'
	vertBlockComb
	(divHoriFromDivDist divE)
	(renderToBlockWithSize rndrWithSizePStd{ fillTile="+"})
	(renderToBlockWithSize rndrWithSizePStd{ fillTile="*"})

horiWithSize :: (Show src) => RenderMethodWithSize [src] (Block Char) (Size Int) Int
horiWithSize = combineWithSize hori
	(divHoriFromDivDist divE)
	(repeat $ renderToBlockWithSize rndrWithSizePStd{ fillTile="+"})
	

--testMeth :: (Card n) => n -> RenderFunctionWithSize (src,src) (Block Char) (Size Int)
testMeth indexDim = combineWithSize2 indexDim (divHoriFromDivDist divE) (renderToBlockWithSize rndrWithSizePStd) (renderToBlockWithSize rndrWithSizePStd)

test = (runRenderFWithSize $ testMeth n0) (10,10) (20,30)
-}

--testMethod2 = combWithDist (divVertFromDivDist divE) (renderToVertBlock ".") (renderToVertBlock "*")

divE count width = take count $ repeat 4


{-
type DivSize dist = DivDist (Size dist)
divHoriFromDivDist :: DivDist dist ->  DivSize dist
divHoriFromDivDist divDist count size = zip
	(repeat $ vecY size)
	(divDist count $ vecX size)
divVertFromDivDist :: DivDist dist ->  DivSize dist
divVertFromDivDist divDist count size = zip
	(divDist count $ vecY size)
	(repeat $ vecX size)
-}

chop width list = case list of
	[] -> [] 
	_ -> take width list : chop width (drop width list)

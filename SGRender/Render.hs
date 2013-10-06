{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}
module SGRender.Render where

import SGData.Vector2D
import Card.Card
import Card.Unary
import Data.Monoid
import Control.Applicative

-- |represents a way to combine representations:
type ReprComb repr = repr -> repr -> repr
-- |calculate a representation for source
type RenderFunction src dst params = params -> src -> dst

data RenderMethod src repr params srcInfo = RenderMeth {
	srcInfo :: src -> srcInfo,
	renderF :: RenderFunction src repr params
}

-- |a thing that can be concatenated in many ways:
class MultiMonoid a countDim | a -> countDim where
	--mmempty :: indexDim -> a
	mmappend :: Card n => n -> a -> a -> a

hori = n0
vert = n1

type Binary a = a -> a -> a

--type CombineSrcInfo = [srcInfo] -> 

{-
	1. listConstr
-}

combine :: (srcInfo -> srcInfo -> srcInfo) -> Binary repr -> (param -> [srcInfo] -> [paramSubMeth]) -> [RenderMethod src repr paramSubMeth srcInfo] -> RenderMethod [src] repr param srcInfo
combine concInfo concRepr listParamsFromListSrcInfo rndrMethList = RenderMeth {
	srcInfo = newSrcInfo,
	renderF = newRenderF
} where
	newSrcInfo listSrc = (foldl1 concInfo $ listSrcInfo listSrc)
	newRenderF param listSrc = foldl1 concRepr $ zipWith3 (\params f s -> (renderF f) params s) (listParams param listSrc) rndrMethList listSrc
	listParams param listSrc = listParamsFromListSrcInfo param $ listSrcInfo listSrc
	listSrcInfo listSrc = zipWith (\f s -> (srcInfo f) s) rndrMethList listSrc


type Size a = (a, a)
type Count = Int 



{-
basic :: (Show a) => String -> RenderMethod a String Int Int
basic tile = RenderMeth { srcInfo = length . show, renderF = (\params val -> forceSize params tile $ show val) }

combEqualSize:: (Show a) => RenderMethod [a] String Int Int
combEqualSize = combine (+) (++) pFromSrcInfo (repeat $ basic "_")
	where
		pFromSrcInfo params listSrcInfo = repeat $ maximum listSrcInfo

combDivSize :: (Show a) => RenderMethod [a] String Int Int
combDivSize = combine (+) (++) pFromSrcInfo (repeat $ basic "_")
	where
		pFromSrcInfo params listSrcInfo = repeat $ params `div` length listSrcInfo
combDivSizeWith :: (Show a) => String -> RenderMethod [a] String Int Int
combDivSizeWith sep = combine concInfo concRepr pFromSrcInfo (repeat $ basic "_")
	where
		concInfo l r = l + length sep + r
		concRepr l r = l ++ sep ++ r
		pFromSrcInfo params listSrcInfo = repeat $ (params - (length listSrcInfo - 1)) `div` length listSrcInfo


forceSize size tile str = take size $ str ++ cycle tile
-}

{-
combHori2 = combine2 hori
combVert2 = combine2 vert
-}

{-
combine2 indexDim = combine2' (mmappend indexDim)

combine2' :: ReprComb repr -> RenderFunction srcL repr -> RenderFunction srcR repr -> RenderFunction (srcL,srcR) repr
combine2' conc rndrFuncL rndrFuncR (srcL,srcR) = rndrFuncL srcL `conc` rndrFuncR srcR

combine' :: ReprComb repr -> [RenderFunction src repr] -> RenderFunction [src] repr
combine' conc rndrFuncList srcList = foldl1 conc $ zipWith (\func src -> func src) rndrFuncList srcList

type RenderFunctionWithSize src dst size = size -> src -> dst

data RenderMethodWithSize src dst size area = RenderMethodWithSize {
	runRenderFWithSize :: RenderFunctionWithSize src dst size,
	checkSize :: src -> area
}
type DivDist a = Count -> a -> [a]

{-|1. uses divDist to divide the given size

2. use the 'rndrFunc'tions to render both 'src'es so that they fill the sizes from step 1.

3. concatenate both results using 'mmappend' with 'indexDim'
-}
combineWithSize2 :: (Card countDim, MultiMonoid repr countDim, Card indexDim, Num area) => indexDim -> DivDist dist -> RenderMethodWithSize srcL repr dist area -> RenderMethodWithSize srcR repr dist area -> RenderMethodWithSize (srcL,srcR) repr dist area
combineWithSize2 indexDim = combineWithSize2' (mmappend indexDim)

{-|same as combineWithSize2, but explicitly give a 'concat' method
-}
combineWithSize2' :: (Num area) => ReprComb repr -> DivDist dist -> RenderMethodWithSize srcL repr dist area -> RenderMethodWithSize srcR repr dist area -> RenderMethodWithSize (srcL,srcR) repr dist area
combineWithSize2' concat divDist rndrMethL rndrMethR = RenderMethodWithSize {
	runRenderFWithSize = renderF,
	checkSize = checkSizeNew
} where
	renderF size (srcL,srcR) =
		let
			sizeL : sizeR : _ = divDist 2 size
			(rndrFuncL,rndrFuncR) = (runRenderFWithSize rndrMethL, runRenderFWithSize rndrMethR)
		in
			(rndrFuncL sizeL srcL) `concat` (rndrFuncR sizeR srcR)
	checkSizeNew (srcL,srcR) =
		let
			(checkL,checkR) = (checkSize rndrMethL, checkSize rndrMethR)
		in
			checkL srcL + checkR srcR

combineWithSize :: (Card countDim, MultiMonoid repr countDim, Card indexDim, Num area) => indexDim -> DivDist dist -> [RenderMethodWithSize src repr dist area] -> RenderMethodWithSize [src] repr dist area
combineWithSize indexDim = combineWithSize' (mmappend indexDim)

combineWithSize' :: (Num area) => ReprComb repr -> DivDist dist -> [RenderMethodWithSize src repr dist area] -> RenderMethodWithSize [src] repr dist area
combineWithSize' concat divDist listRndrMeth = RenderMethodWithSize {
	runRenderFWithSize = \size listSrc -> let sizes = divDist (length listSrc) size in
		foldl1 concat $ zipWith3 (\rndrF size src -> rndrF size src) (map runRenderFWithSize listRndrMeth) sizes listSrc, --combineRenderF concat sizes listRndrMeth,
	checkSize = \listSrc -> sum $ zipWith (\checkF src -> checkF src) (map checkSize listRndrMeth) listSrc
}



{-
combine :: (Num area) => ReprComb repr -> [dist] -> [RenderMethodWithSize srcL repr dist area] -> RenderMethodWithSize [src] repr dist area
combine concat sizeList rndrMethList = case sizeList of
	[] -> \
	let
		= divDist 2 size
		(rndrFuncL,rndrFuncR) = (runRenderFWithSize rndrMethL, runRenderFWithSize rndrMethR)
	in
		(rndrFuncL sizeL srcL) `concat` (rndrFuncR sizeR srcR)
-}


{-
hori :: Monoid repr => DivDist dist -> RenderMethodWithSize srcL repr size -> RenderMethodWithSize srcR repr size -> RenderMethodWithSize (srcL,srcR) repr size
hori divDist rndrFuncL rndrFuncR size (srcL,srcR) = let sizeL : sizeR : _ = (divHoriFromDivDist divDist) 2 size in
	(rndrFuncL sizeL srcL) `mappend` (rndrFuncR sizeR srcR)
-}
-}

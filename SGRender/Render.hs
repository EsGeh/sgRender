{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}
module SGRender.Render where

import Util.Vector2D
import Util.Card.Card
import Util.Card.Unary
import Data.Monoid

-- |represents a way to combine representations:
type ReprComb repr = repr -> repr -> repr
-- |calculate a representation for source
type RenderFunction src dst = src -> dst

-- |a thing that can be concatenated in many ways:
class MultiMonoid a countDim | a -> countDim where
	--mmempty :: indexDim -> a
	mmappend :: Card n => n -> a -> a -> a

hori = n0
vert = n1


combine2 :: ReprComb repr -> RenderFunction srcL repr -> RenderFunction srcR repr -> RenderFunction (srcL,srcR) repr
combine2 conc rndrFuncL rndrFuncR (srcL,srcR) = rndrFuncL srcL `conc` rndrFuncR srcR


data RenderFunctionWithSize src dst size area = RenderFunctionWithSize {
	runRenderFWithSize :: size -> src -> dst,
	checkSize :: src -> area
}
type DivDist a = Count -> a -> [a]

{-|1. uses divDist to divide the given size

2. use the 'rndrFunc'tions to render both 'src'es so that they fill the sizes from step 1.

3. concatenate both results using 'mmappend' with 'indexDim'
-}
combineWithSize2 :: (Card countDim, MultiMonoid repr countDim, Card indexDim, Num area) => indexDim -> DivDist dist -> RenderFunctionWithSize srcL repr dist area -> RenderFunctionWithSize srcR repr dist area -> RenderFunctionWithSize (srcL,srcR) repr dist area
combineWithSize2 indexDim = combineWithSize2' (mmappend indexDim)

{-|same as combineWithSize2, but explicitly give a 'concat' method
-}
combineWithSize2' :: (Num area) => ReprComb repr -> DivDist dist -> RenderFunctionWithSize srcL repr dist area -> RenderFunctionWithSize srcR repr dist area -> RenderFunctionWithSize (srcL,srcR) repr dist area
combineWithSize2' concat divDist rndrMethL rndrMethR = RenderFunctionWithSize {
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
			

type Size a = (a, a)
type Count = Int 

{-
hori :: Monoid repr => DivDist dist -> RenderFunctionWithSize srcL repr size -> RenderFunctionWithSize srcR repr size -> RenderFunctionWithSize (srcL,srcR) repr size
hori divDist rndrFuncL rndrFuncR size (srcL,srcR) = let sizeL : sizeR : _ = (divHoriFromDivDist divDist) 2 size in
	(rndrFuncL sizeL srcL) `mappend` (rndrFuncR sizeR srcR)
-}

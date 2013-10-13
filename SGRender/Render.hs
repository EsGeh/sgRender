{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}
module SGRender.Render(
	-- * RenderMethods
	ReprComb,RenderFunction,RenderMethod(..),
	FillFunction,
	-- * combinator
	combine, combine2,
	-- * type class for representations
	MultiMonoid(..),dimX,dimY,
	--Binary(..),Size,Count,
) where

import SGData.Vector2D
import SGCard.Card
import SGCard.Unary
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

type Binary a = a -> a -> a
type FillFunction param repr = param -> repr

--type CombineSrcInfo = [srcInfo] -> 

{-
	1. listConstr
-}

combine :: Binary repr -> FillFunction param repr -> (param -> [srcInfoSub] -> [paramSubMeth]) -> ([srcInfoSub] -> srcInfo) 
	-> [RenderMethod src repr paramSubMeth srcInfoSub]
	-> RenderMethod [src] repr param srcInfo
combine concRepr fillEmpty listParamsFromListSrcInfo concInfo rndrMethList = RenderMeth {
	srcInfo = newSrcInfo,
	renderF = newRenderF
} where
	newSrcInfo listSrc = concInfo $ listSrcInfo listSrc --(foldl1 concInfo $ listSrcInfo listSrc)
	newRenderF param listSrc = case listSrc of
		[] -> fillEmpty param
		_ -> foldl1 concRepr $ zipWith3 (\params f s -> (renderF f) params s) (listParams param listSrc) rndrMethList listSrc
	listParams param listSrc = listParamsFromListSrcInfo param $ listSrcInfo listSrc
	listSrcInfo listSrc = zipWith (\f s -> (srcInfo f) s) rndrMethList listSrc

combine2 :: (reprL -> reprR -> repr) -> (param -> (srcInfoSubL,srcInfoSubR) -> (paramSubMethL,paramSubMethR))-> (srcInfoSubL -> srcInfoSubR -> srcInfo) 
	-> RenderMethod srcL reprL paramSubMethL srcInfoSubL 
	-> RenderMethod srcR reprR paramSubMethR srcInfoSubR
	-> RenderMethod (srcL,srcR) repr param srcInfo
combine2 concRepr paramsFromListSrcInfo concInfo rndrMethL rndrMethR = RenderMeth {
	srcInfo = newSrcInfo,
	renderF = newRenderF
} where
	newSrcInfo sources = (uncurry concInfo) (sourcesInfo sources) --(foldl1 concInfo $ listSrcInfo listSrc)
	--newRenderF :: 
	newRenderF param sources@(srcL,srcR) = 
		concRepr 
			((renderF rndrMethL) (fst $ paramSub param sources) srcL)
			((renderF rndrMethR) (snd $ paramSub param sources) srcR)
		--foldl1 concRepr $ zipWith3 (\params f s -> (renderF f) params s) (listParams param listSrc) rndrMethList listSrc
	paramSub param sources = paramsFromListSrcInfo param $ sourcesInfo sources
	sourcesInfo (srcL,srcR) = ((srcInfo rndrMethL) srcL, (srcInfo rndrMethR) srcR) 

-- |a thing that can be concatenated in many ways:
class MultiMonoid a countDim | a -> countDim where
	--mmempty :: indexDim -> a
	mmappend :: Card n => n -> a -> a -> a

dimX = n0
dimY = n1

combineMM dim = combine (mmappend dim)

type Size a = (a, a)
type Count = Int 

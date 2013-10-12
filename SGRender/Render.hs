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

mapOnRenderF newRenderF renderMeth = renderMeth{
	renderF = newRenderF (renderF renderMeth)
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

combine :: ([srcInfoSub] -> srcInfo) -> Binary repr -> (param -> repr) -> (param -> [srcInfoSub] -> [paramSubMeth]) 
	-> [RenderMethod src repr paramSubMeth srcInfoSub]
	-> RenderMethod [src] repr param srcInfo
combine concInfo concRepr fillEmpty listParamsFromListSrcInfo rndrMethList = RenderMeth {
	srcInfo = newSrcInfo,
	renderF = newRenderF
} where
	newSrcInfo listSrc = concInfo $ listSrcInfo listSrc --(foldl1 concInfo $ listSrcInfo listSrc)
	newRenderF param listSrc = case listSrc of
		[] -> fillEmpty param
		_ -> foldl1 concRepr $ zipWith3 (\params f s -> (renderF f) params s) (listParams param listSrc) rndrMethList listSrc
	listParams param listSrc = listParamsFromListSrcInfo param $ listSrcInfo listSrc
	listSrcInfo listSrc = zipWith (\f s -> (srcInfo f) s) rndrMethList listSrc

combine2 :: (srcInfoSubL -> srcInfoSubR -> srcInfo) -> (reprL -> reprR -> repr) -> (param -> (srcInfoSubL,srcInfoSubR) -> (paramSubMethL,paramSubMethR))
	-> RenderMethod srcL reprL paramSubMethL srcInfoSubL 
	-> RenderMethod srcR reprR paramSubMethR srcInfoSubR
	-> RenderMethod (srcL,srcR) repr param srcInfo
combine2 concInfo concRepr paramsFromListSrcInfo rndrMethL rndrMethR = RenderMeth {
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

type Size a = (a, a)
type Count = Int 


{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}
{-| this module defines what the other modules of this library are based on

 * basic data types

 * pattern how to combine "RenderMethods"

-}
module SGRender.Render(
	-- * important types
	ReprComb,RenderFunction,RenderMethod(..),
	--renderF,srcInfo,
	FillFunction,
	-- * pseudo constructors
	renderMeth,
	-- * basic RenderMethods
	renderToConstRepr,
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

{-|
<<diagrams/renderMethod.svg>>
-}
data RenderMethod src repr params srcInfo = RenderMeth {
	renderF :: RenderFunction src repr params, -- ^a way to calculate a representation of a source
	srcInfo :: src -> srcInfo -- ^ usually returns how much space the repr will need
}
-- | create a render method
renderMeth newRenderF newSrcInfo = RenderMeth {
	srcInfo = newSrcInfo,
	renderF = newRenderF
}

type Binary a = a -> a -> a
type FillFunction param repr = param -> repr

-- | ignores the source, and just applies reprFromParam
renderToConstRepr :: (param -> repr) -> (src -> info) -> RenderMethod src repr param info
renderToConstRepr reprFromParam infoFromSrc = renderMeth renderF infoFromSrc
	where
		renderF param _ = reprFromParam param

--type CombineSrcInfo = [srcInfo] -> 

{-
	. listConstr
-}

{-| combine a ["RenderMethod"] to one "RenderMethod"

The new Rendermethod behaves as follows (simplified!):

Pseudo-Code:
	
@
given a [src]
	if listSrc is empty:
		use fillEmpty param
	else
		map ["RenderMethods"] to the [src]
@

more detailed explanation to come soon ...

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

{-| combine two "RenderMethod"s one "RenderMethod"

<<diagrams/combine2.svg>> 

 -}
combine2 :: (reprL -> reprR -> repr) -> (param -> (srcInfoSubL,srcInfoSubR) -> (paramSubMethL,paramSubMethR))-> (srcInfoSubL -> srcInfoSubR -> srcInfo) 
	-> RenderMethod srcL reprL paramSubMethL srcInfoSubL 
	-> RenderMethod srcR reprR paramSubMethR srcInfoSubR
	-> RenderMethod (srcL,srcR) repr param srcInfo
combine2 concRepr paramsFromListSrcInfo concInfo rndrMethL rndrMethR = RenderMeth {
	srcInfo = newSrcInfo,
	renderF = newRenderF
} where
	newSrcInfo sources = (uncurry concInfo) (sourcesInfo sources)
	--newRenderF :: 
	newRenderF param sources@(srcL,srcR) = 
		concRepr 
			((renderF rndrMethL) (fst $ paramSub param sources) srcL)
			((renderF rndrMethR) (snd $ paramSub param sources) srcR)
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

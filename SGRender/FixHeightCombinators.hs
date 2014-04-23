module SGRender.FixHeightCombinators(
	-- ** with fix height
	renderToBlockFix,
	renderListHoriFix, renderListVertFix,
	renderListHoriFixWithSep,
	-- * ready to use renderMethods
	renderTable,
	renderTree,
	-- * render with default size
	renderSqueezed, renderMinSize,
) where


import SGRender.BlockCombinators
import SGRender.Render
import SGRender.Block
import SGRender.DivFunctions
import SGData hiding(Width,Height)

import Control.Applicative
import Data.List
import Debug.Trace

x = 0
y = 1


renderToBlockFix :: Show src => Height -> RenderToBlockParams src char -> RenderMethod src (Block char) (Size Int) (Size Int)
renderToBlockFix height params = renderMeth (renderF $ renderM) calcSrcInfo
	where
		renderM = renderToBlock params
		calcSrcInfo src = (srcInfo renderM src (y,height), height)

renderMDimRelFromSize renderM = renderMeth (renderF renderM) calcDimRel
	where
		calcDimRel src defOneDim = case fst defOneDim of 
			0 -> vecY size
			1 -> vecX size
			where
				size = (srcInfo renderM src)

renderListHoriFix :: Show src => DivBlocks Int -> [RenderMethod src (Block Char) (Size Int) (Size Int)] -> RenderMethod [src] (Block Char) (Size Int) (Size Int)
renderListHoriFix div listRenderMeth = renderMeth (renderF renderHori) calcSize
	where
		renderHori = renderListHori div	$ map renderMDimRelFromSize listRenderMeth
		calcSize src = ((srcInfo renderHori) src (y,1), (srcInfo renderHori) src (x,1))
renderListVertFix :: Show src => DivBlocks Int -> [RenderMethod src (Block Char) (Size Int) (Size Int)] -> RenderMethod [src] (Block Char) (Size Int) (Size Int)
renderListVertFix div listRenderMeth = renderMeth (renderF renderVert) calcSize
	where
		renderVert= renderListVert div $ map renderMDimRelFromSize listRenderMeth
		calcSize src = ((srcInfo renderVert) src (y,1), (srcInfo renderVert) src (x,1))

renderListHoriFixWithSep :: Show src => FillFunction (Size Int) (Block Char) -> Width -> DivBlocks Int -> [RenderMethod src (Block Char) (Size Int) (Size Int)] -> RenderMethod [src] (Block Char) (Size Int) (Size Int)
renderListHoriFixWithSep sepFillF sepWidth div listRenderMeth = renderMeth (renderF renderHori) calcSize
	where
		renderHori = renderListHoriWithSep sepFillF sepWidth div $ map renderMDimRelFromSize listRenderMeth
		calcSize src = ((srcInfo renderHori) src (y,1), (srcInfo renderHori) src (x,1))
renderListVertFixWithSep :: Show src => FillFunction (Size Int) (Block Char) -> Height -> DivBlocks Int -> [RenderMethod src (Block Char) (Size Int) (Size Int)] -> RenderMethod [src] (Block Char) (Size Int) (Size Int)
renderListVertFixWithSep sepFillF sepHeight div listRenderMeth = renderMeth (renderF renderVert) calcSize
	where
		renderVert = renderListVertWithSep sepFillF sepHeight div $ map renderMDimRelFromSize listRenderMeth
		calcSize src = ((srcInfo renderVert) src (y,1), (srcInfo renderVert) src (x,1))



--type Width = Int


--debug info x = x
debug info x = trace (info ++ show x) x






testTree = node 1 $ [ leaf 1.1, leaf 1.2, node 1.3 [ leaf 2, leaf 3] ]

renderTree :: Show src => RenderMethod (Tree src) (Block Char) (Size Int) (Size Int)
renderTree = renderMeth newRenderF newSrcInfo
	where
		newRenderF size src = (renderF $ renderHeadAndSubTrees) size $
			(value src, children src)
		newSrcInfo tree = (srcInfo renderHeadAndSubTrees) (value tree, children tree)

{- |@
renderSqueezed dim renderMeth src
@

tries to minimize the space needed in the dimension 'dim'

note for developers (Pseudo-Code):

@
renderSqueezed x renderMethod = 
	distY = (srcInfo renderMethod :: DimRel) (x,1)
	distX = (srcInfo renderMethod :: DimRel) (y,distY)
@

-- this means:

for every

@
renderMethod :: 'RenderMethod' src (Block char) (DimRel Int) (Size Int)
@

the following must be true:

@
	(srcInfo renderMethod) (dim, ? <=1) == (srcInfo renderMethod) (dim, 1)
@

-}
renderSqueezed :: IndexDim -> RenderMethod src repr (Size Int) (DimRel Int) -> src -> repr
renderSqueezed minDim renderMeth src = (renderF renderMeth) size src
	where
		size :: Size Int
		size = squeezedSize minDim (srcInfo renderMeth src)

squeezedSize indexDim dimRel = sizeFromDimRel dimRel (1-indexDim, dimRel (indexDim,1))

--type IndexDim = Int

renderMinSize :: (Show src) => RenderMethod src repr srcInfo srcInfo -> src -> repr
renderMinSize renderMeth src = (renderF renderMeth) (srcInfo renderMeth src) src

renderHeadAndSubTrees :: Show src => RenderMethod (src,[Tree src]) (Block Char) (Size Int) (Size Int)
renderHeadAndSubTrees = combine2
	(mmappend dimY)
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
				heightHead : heightChildren : _ = divDistCut (vecY wholeSize)
					[1, vecY sizeD]
				--((widthAllInAll,1),widthAllInAll) 

renderSubTrees :: Show src => RenderMethod [Tree src] (Block Char) (Size Int) (Size Int)
renderSubTrees = renderMeth (renderF renderHori) (srcInfo renderHori)
	where
		renderHori = renderListHoriFixWithSep (filledBlock "|") 1 divBlocks (repeat renderTree)

{-horizontal (\size -> filledBlock " " size)
	calcSubParams
	(\listSize -> case listSize of
		[] -> (0,0)
		_ -> foldl1 (\l r -> ((vecX l) + (vecX r), max (vecY l) (vecY r))) $
			listSize)
			--(maximum listWidth,length listWidth)) 
	(repeat renderTree)
	where
		calcSubParams size listSize = zip
			(divDistEqual (vecX size) (map vecX listSize))
			(repeat $ vecY size)
-}

{-
renderListVert :: Show src => DivBlocks Int -> [RenderMethod src (Block Char) (Size Int) (DimRel Int)] -> RenderMethod [src] (Block Char) (Size Int) (DimRel Int)
renderListVert divBlocks listRenderMeth = combine
	(mmappend dimY)
	(\size -> filledBlock " " size) calcSize
	calcInfo
	listRenderMeth
	where
		calcSize :: Size Int -> [DimRel Int] -> [Size Int]
		calcSize size listDimRel = zip
			(repeat (vecX size))
			(divDist (vecY size) $ take (length listDimRel) $ repeat height)
		calcInfo listDimRel defOneDim = case fst defOneDim of
			0 -> height*(length listDimRel)
			1 ->  maximum $ listDimRel <*> [(1,height)]

testGenVert :: Show src => RenderMethod [src] (Block Char) (Size Int) (DimRel Int)
testGenVert = renderListVert 1 divDistEqual (repeat $ renderToBlock renderToBlockParamsStd)

testGen :: Show src => RenderMethod [[src]] (Block Char) (Size Int) (DimRel Int)
testGen = renderListHori divTest $
	repeat $ renderListVert divTest $
		repeat $ renderToBlock renderToBlockParamsStd
-}

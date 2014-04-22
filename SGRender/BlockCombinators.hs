module SGRender.BlockCombinators(
	-- * render combinators (combine a list of RenderMethods)
	renderListHori,renderListVert,
	renderListHoriWithSep, renderListVertWithSep,
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
import SGRender.Render
import SGRender.Block
import SGRender.DivFunctions
import SGData hiding(Width,Height)

import Control.Applicative
import Data.List
import Debug.Trace


x = 0
y = 1


renderListHori :: Show src => DivBlocks Int -> [RenderMethod src (Block Char) (Size Int) (DimRel Int)] -> RenderMethod [src] (Block Char) (Size Int) (DimRel Int)
renderListHori divBlocks listRenderMeth = combine
	(mmappend dimX)			-- concRepr
	(\size -> filledBlock " " size) -- fillFunction
	calcSize
	calcInfo
	listRenderMeth
	where
		calcSize :: Size Int -> [DimRel Int] -> [Size Int]
		calcSize size listDimRel = zip
			(divBlocks (x, vecX size) listDimRel)
			(repeat $ vecY size)
		-- calcInfo :: [DimRel Int] -> DimRel Int
		calcInfo listDimRel defOneDim = case fst defOneDim of
			0 -> case listDimRel of
				[] -> 0
				_ -> maximum $ listHeight $ listWidth
					where
						listWidth = (divBlocks defOneDim $ listDimRel)
						listHeight listWidth = getZipList $ 
							ZipList listDimRel
							<*>
							ZipList (zip (repeat x) listWidth)
			1 -> sum $ listDimRel <*> [defOneDim]
renderListVert :: Show src => DivBlocks Int -> [RenderMethod src (Block Char) (Size Int) (DimRel Int)] -> RenderMethod [src] (Block Char) (Size Int) (DimRel Int)
renderListVert divBlocks listRenderMeth = combine
	(mmappend dimY)
	(\size -> filledBlock " " size)
	calcSize
	calcInfo
	listRenderMeth
	where
		calcSize :: Size Int -> [DimRel Int] -> [Size Int]
		calcSize size listDimRel = zip
			(repeat $ vecX size)
			(divBlocks (y, vecY size) listDimRel)
		-- calcInfo :: [DimRel Int] -> DimRel Int
		calcInfo listDimRel defOneDim = case fst defOneDim of
			0 -> sum $ listDimRel <*> [defOneDim]
			1 -> case listDimRel of
				[] -> 0
				_ -> maximum $ listWidth $ listHeight
					where
						listHeight = divBlocks defOneDim $ listDimRel
						listWidth listHeight = getZipList $ 
							ZipList listDimRel
							<*>
							ZipList (zip (repeat y) listHeight)

renderListHoriWithSep :: Show src => FillFunction (Size Int) (Block Char) -> Width -> DivBlocks Int 
	-> [RenderMethod src (Block Char) (Size Int) (DimRel Int)]
	-> RenderMethod [src] (Block Char) (Size Int) (DimRel Int)
renderListHoriWithSep fillFSep sepWidth divBlocks listRenderMeth =
	renderMeth
		(\size src -> renderF renderInterspersed' size (interspersedSrc src))	-- renderF
		(\src -> srcInfo renderInterspersed' (interspersedSrc src))		-- srcInfo
	where
		renderInterspersed' = renderListHori (divWithSep sepWidth divBlocks) $ renderInterspersed renderSep listRenderMeth
			where
				renderSep = renderToConstRepr fillFSep dimRelSep
				dimRelSep src defOneDim = case fst defOneDim of
					0 -> 0
					1 -> sepWidth
						{-case snd defOneDim of
						0 -> 0
						_ -> sepWidth-}
		interspersedSrc listSrc = intersperse (Left ()) $ map Right listSrc
renderListVertWithSep :: Show src => FillFunction (Size Int) (Block Char)  -> Width -> DivBlocks Int -> [RenderMethod src (Block Char) (Size Int) (DimRel Int)] -> RenderMethod [src] (Block Char) (Size Int) (DimRel Int)
renderListVertWithSep fillFSep sepWidth divBlocks listRenderMeth = renderMeth (\size src -> renderF renderInterspersed' size (interspersedSrc src)) (\src -> srcInfo renderInterspersed' (interspersedSrc src))
	where
		renderInterspersed' = renderListVert (divWithSep sepWidth divBlocks) $ renderInterspersed renderSep listRenderMeth
			where
				renderSep = renderToConstRepr fillFSep dimRelSep
				dimRelSep src defOneDim = case fst defOneDim of
					0 -> case snd defOneDim of
						0 -> 0
						_ -> sepWidth

					1 -> sepWidth 
					{-
					0 -> 0
					1 -> case snd defOneDim of
						0 -> 0
						_ -> sepWidth
				-}
		interspersedSrc listSrc = intersperse (Left ()) $ map Right listSrc
	
divWithSep sepWidth divF (dim, dist) listDimRel = divDistEqual dist $
	intersperse sepWidth $
		divF (dim, dist - ((length listDimRel - 1) `div` 2)*sepWidth) $ 
				(unintersperse listDimRel)

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


renderEither :: (RenderMethod srcLeft repr param info) -> (RenderMethod srcRight repr param info) -> RenderMethod (Either srcLeft srcRight) repr param info
renderEither renderLeft renderRight = renderMeth newRenderF newSrcInfo
	where
		newRenderF param src = case src of
			Left srcLeft -> renderF renderLeft param srcLeft
			Right srcRight -> renderF renderRight param srcRight
		newSrcInfo src = case src of
			Left srcLeft -> srcInfo renderLeft srcLeft
			Right srcRight -> srcInfo renderRight srcRight

--type Width = Int

renderInterspersed :: Show src => RenderMethod srcSep (Block Char) (Size Int) (DimRel Int) -> [RenderMethod src (Block Char) (Size Int) (DimRel Int)]
	-> [RenderMethod (Either srcSep src) (Block Char) (Size Int) (DimRel Int)]
renderInterspersed renderSep listRenderMeth = intersperse renderMaybeSep listRenderMaybe
	where
		--renderMaybeSep :: RenderMethod (Either srcSep src) (Block Char) (Size Int) (DimRel Int)
		renderMaybeSep = renderEither renderSep (renderError "not a seperator!")
		--listRenderMaybe :: [RenderMethod (Either srcSep src) (Block Char) (Size Int) (DimRel Int)]
		listRenderMaybe = map toMaybe listRenderMeth
			where
				toMaybe renderMeth = renderEither (renderError "seperator!") renderMeth
		renderError errorMsg = renderMeth (\param src -> error errorMsg) (\src -> error errorMsg)


unintersperse list = case list of
	[] -> []
	[x] -> [x]
	fst:snd:rest -> fst : unintersperse rest

--debug info x = x
debug info x = trace (info ++ show x) x




renderTable:: Show src => RenderMethod [[src]] (Block Char) (Size Int) (DimRel Int)
renderTable = renderListHori divBlocks $
	repeat $ renderListVert divBlocks $
		repeat $ renderToBlock renderToBlockParamsStd


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

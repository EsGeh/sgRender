
import SGRender.BlockCombinators
import SGRender.Render
import SGRender.Block
import SGRender.DivFunctions

import Test.QuickCheck


prop_dimRel dimRel value =
	prop_dimRelX dimRel value
	&&
	prop_dimRelY dimRel value
prop_dimRelX dimRel value =
	let
		distY = dimRel (x, value)
		distY' = dimRel (x, dimRel (y, distY) )
	in
		distY == distY'
prop_dimRelY dimRel value =
	let
		distX = dimRel (y, value)
		distX' = dimRel (y, dimRel (x, distX) )
	in
		distX == distX'

test = do
	--testVert
	testVertAfterHori

testVert = check $ prop_dimRel (srcInfo vert [1,2,33])
testVertAfterHori = check $ prop_dimRel (srcInfo (renderListVert divBlocks $ repeat hori) [[1,2,33],[4,5,6]])

check = quickCheckWith stdArgs{ maxSuccess = 1000 }


hori :: Show src => RenderMethod [src] (Block Char) (Size Int) (DimRel Int)
hori = renderListHori divBlocks (repeat $ renderToBlock renderToBlockParamsStd)
horiWithSep :: Show src => RenderMethod [src] (Block Char) (Size Int) (DimRel Int)
horiWithSep = renderListHoriWithSep (filledBlock "|") 1 divBlocks (repeat $ renderToBlock renderToBlockParamsStd)

vert :: Show src => RenderMethod [src] (Block Char) (Size Int) (DimRel Int)
vert = renderListVert divBlocks (repeat $ renderToBlock renderToBlockParamsStd)
vertWithSep :: Show src => RenderMethod [src] (Block Char) (Size Int) (DimRel Int)
vertWithSep = renderListVertWithSep (filledBlock "-") 1 divBlocks (repeat $ renderToBlock renderToBlockParamsStd)

tryRenderMeth :: Show src => RenderMethod src (Block Char) (Size Int) (DimRel Int) -> src -> IO ()
tryRenderMeth renderMeth src = do
	--putStrLn $ showDimRel ((srcInfo renderMeth) src)
	putStrLn $ "x = [0..10] -> y:"
	putStrLn $ show $ map ((srcInfo renderMeth) src) (zip (repeat x) [0..10])
	putStrLn $ "y = [0..10] -> x:"
	putStrLn $ show $ map ((srcInfo renderMeth) src) (zip (repeat y) [0..10])
	putStrLn $ "renderF:"
	putStrLn $ show $ (renderF renderMeth) (20,4) src

showDimRel dimRel =
	let
		listX = [0..10]
		listY = [0..10]
	in
		"x->y: " ++ (showList $ listTupel x $ listX)
		where
			listTupel indexDim list = list `zip` (map dimRel $ zip (repeat indexDim) list)
			showList listTupel = foldl1 conc $ map (\(f, s) -> show f ++ "->" ++ show s) listTupel
				where conc l r = l ++ ", " ++ r

x = 0
y = 1

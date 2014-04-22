
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

tableData = [[1,2,33],[4,5,6]]
tableDimRel = (srcInfo renderTable [[1,2,33],[4,5,6]])
test = do
	let dimRelHori = (srcInfo hori [1,2,33])
	putStrLn "test hori:"
	putStrLn $ showDimRel dimRelHori
	putStrLn "check prop_dimRelX:"
	check $ prop_dimRelX dimRelHori
	putStrLn "check prop_dimRelY:"
	check $ prop_dimRelY dimRelHori

	let dimRelVert = (srcInfo vert [1,2,33])
	putStrLn "test vert:"
	putStrLn $ showDimRel dimRelVert
	putStrLn "check prop_dimRelX:"
	check $ prop_dimRelX dimRelVert
	putStrLn "check prop_dimRelY:"
	check $ prop_dimRelY dimRelVert
	--testVertAfterHori

check = quickCheckWith stdArgs{ maxSuccess = 1000 }


hori :: Show src => RenderMethod [src] (Block Char) (Size Int) (DimRel Int)
hori = renderListHori divBlocks (repeat $ renderToBlock renderToBlockParamsStd)
horiWithSep :: Show src => RenderMethod [src] (Block Char) (Size Int) (DimRel Int)
horiWithSep = renderListHoriWithSep (filledBlock "|") 1 divBlocks (repeat $ renderToBlock renderToBlockParamsStd)

vert :: Show src => RenderMethod [src] (Block Char) (Size Int) (DimRel Int)
vert = renderListVert divBlocks (repeat $ renderToBlock renderToBlockParamsStd)
vertWithSep :: Show src => RenderMethod [src] (Block Char) (Size Int) (DimRel Int)
vertWithSep = renderListVertWithSep (filledBlock "-") 1 divBlocks (repeat $ renderToBlock renderToBlockParamsStd)

{-
renderTable:: Show src => RenderMethod [[src]] (Block Char) (Size Int) (DimRel Int)
renderTable = renderListVert divBlocks $
	repeat $ renderListVert divBlocks $
		repeat $ renderToBlock renderToBlockParamsStd
-}

tryRenderMeth :: Show src => RenderMethod src (Block Char) (Size Int) (DimRel Int) -> src -> IO ()
tryRenderMeth renderMeth src = do
	{-putStrLn $ "x = [0..10] -> y:"
	putStrLn $ show $ map ((srcInfo renderMeth) src) (zip (repeat x) [0..10])
	putStrLn $ "y = [0..10] -> x:"
	putStrLn $ show $ map ((srcInfo renderMeth) src) (zip (repeat y) [0..10])-}
	putStrLn "dimRel:"
	putStrLn $ showDimRel (srcInfo renderMeth src)
	putStrLn $ "render to (20,4):"
	putStrLn $ show $ (renderF renderMeth) (20,4) src

showDimRel :: DimRel Int -> String
showDimRel dimRel =
	let
		listX = [0..9]
		listY = [0..9]
	in
		"x->y:\n" ++ show listX ++ "\n" ++ show (listTupel x listX)
		++
		"\ny->x:\n" ++ show listY ++ "\n" ++ show (listTupel y listY)
		where
			listTupel indexDim list = (map dimRel $ zip (repeat indexDim) list)
			showList listTupel = foldl1 conc $ map (\(f, s) -> show f ++ "->" ++ show s) listTupel
				where conc l r = l ++ ", " ++ r

x = 0
y = 1

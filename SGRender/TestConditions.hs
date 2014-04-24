module SGRender.TestConditions(
	prop_dimRelX, prop_dimRelY, prop_dimRel,
	test,
	tryRenderMeth,
	showDimRel,
) where

import SGRender.Render
import SGRender.Block
import SGRender.DivFunctions

import Test.QuickCheck

{- |DimRel-Condition (<==> DimRel-Condition-X && DimRel-ConditionY)
-}
prop_dimRel :: DimRel Int -> Int -> Bool
prop_dimRel dimRel value =
	prop_dimRelX dimRel value
	&&
	prop_dimRelY dimRel value

{- |DimRel-ConditionX 
x -> y -> x' -> y' ==> y == y'
-}
prop_dimRelX :: DimRel Int -> Int -> Bool
prop_dimRelX dimRel value =
	let
		distY = dimRel (x, value)
		distY' = dimRel (x, dimRel (y, distY) )
	in
		distY == distY'

{- |DimRel-ConditionY
y -> x -> y' -> x' ==> x == x'
-}
prop_dimRelY :: DimRel Int -> Int -> Bool
prop_dimRelY dimRel value =
	let
		distX = dimRel (y, value)
		distX' = dimRel (y, dimRel (x, distX) )
	in
		distX == distX'

--tableData = [[1,2,33],[4,5,6]]
--tableDimRel = (srcInfo renderTable [[1,2,33],[4,5,6]])

{- | 'test renderMeth src' does the following:

1. 'tryRenderMeth' renderMeth src

2. check 'prop_dimRelX'

3. check 'prop_dimRelY'

-}
test renderMeth src = do
	let dimRel = (srcInfo renderMeth src)
	tryRenderMeth renderMeth src
	putStrLn "check prop_dimRelX:"
	check $ prop_dimRelX dimRel
	putStrLn "check prop_dimRelY:"
	check $ prop_dimRelY dimRel
	--testVertAfterHori

check = quickCheckWith stdArgs{ maxSuccess = 1000 }

{-
renderTable:: Show src => RenderMethod [[src]] (Block Char) (Size Int) (DimRel Int)
renderTable = renderListVert divBlocks $
	repeat $ renderListVert divBlocks $
		repeat $ renderToBlock renderToBlockParamsStd
-}

{- |'tryRenderMeth renderMeth src' does the following:

1. 'showDimRel' (srcInfo renderMeth src)

2. show $ (renderF renderMeth) (20,4) src

-}
tryRenderMeth :: Show src => RenderMethod src (Block Char) (Size Int) (DimRel Int) -> src -> IO ()
tryRenderMeth renderMeth src = do
	putStrLn "dimRel:"
	putStrLn $ showDimRel (srcInfo renderMeth src)
	putStrLn $ "render to (20,4):"
	putStrLn $ show $ (renderF renderMeth) (20,4) src

{- |helpful for debugging: give a string-representation for a DimRel -}
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

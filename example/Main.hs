import SGRender.Render
import SGRender.Block
import SGRender.BlockCombinators

-- "renderToBlock" is a basic "RenderMethod", to render things to a "Block" of text:
renderMethSimple :: Show src => RenderMethod src (Block Char) (Size Int) (DimRel Int)
renderMethSimple = renderToBlock renderToBlockParamsStd

-- this is how to "execute" such a render function to render Text to "Block" of text:
example1 = (renderF renderMethSimple)
	(10,10)				-- the size to be filled (<lineLength>, <number of lines>)
	"Hello World!"

-- of course this works for anything, that implements the "Show" type class, e.g. Integers:
example1' = (renderF renderMethSimple)
	(10,10)				-- the size to be filled (<lineLength>, <number of lines>)
	1302050347895018239765849823749 -- a very long number

-- notice how the source is "wrapped" to make its representation fit into the resulting "Block"


-- if we know how to render one thing to a Block,
-- we can easily define how to render many things to a Block as well:
renderMethHori :: Show src => RenderMethod [src] (Block Char) (Size Int) (DimRel Int)
renderMethHori = renderListHoriWithSep
	(filledBlock "|")	-- seperator
	1			-- width of the seperator
	divBlocks		-- the strategy to divide the space given
	(repeat renderMethSimple)	-- a list of "RenderMethod"s to combine

renderMethVert :: Show src => RenderMethod [src] (Block Char) (Size Int) (DimRel Int)
renderMethVert = renderListVert
	divBlocks		-- the strategy to divide the space given
	(repeat renderMethSimple)	-- a list of "RenderMethod"s to combine


-- examples on how to "execute" these new "RenderMethod"s on lists of numbers
example2 = (renderF renderMethHori)
	(15,2)			-- size to fill
	[101, 0, 1000004, -4]

example3 = (renderF renderMethVert)
	(15,10)			-- size to fill
	[101, 0, 1000004, -4]

-- if we choose a to small value for the size to be filled, the output is cut.
-- But how to know which size the output will take?
-- we can't know that in advance, or can we?

-- we can. The following example shows how to calculate
-- how much space is needed in x direction
-- if we want to use 2 lines:
example4 = (srcInfo renderMethSimple) -- :: DimRel Int
	"Hello World" (y,2)	-- two lines

-- srcInfo applied on a "RenderMethod" gives you a function
-- that can be used to retrieve some information from the source,
-- in this case: a relation between the two sides of the minimum rectangle
-- the result will fill without being cut


-- if you want to render somthing by using minimum size
-- you can use "renderSqueezed"
example5 = renderSqueezed y renderMethSimple "Hello World" -- minimize the space to use in y-direction (number of lines, that is)
example5' = renderSqueezed x renderMethSimple "Hello World" -- minimize the space to use in x-direction (line length, that is)line length, that is)

example6 = renderSqueezed y renderMethHori 
	[101, 0, 1000004, -4]
example6' = renderSqueezed x renderMethHori 
	[101, 0, 1000004, -4]

x = 0
y = 1

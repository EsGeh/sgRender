# Introduction

## Basic Idea

- serialisation of some *source* to some *representation* (this process is called "rendering")

- Combinators (if you can render one thing, you a can render many)
- Formatting
	- make something fill a certain space
	- -> how to know about how much space some input needs minimum?


0. (Serialisation)

	show :: src -> repr

1. (Combinators) If you can render one thing, you a can render many
why? 
because usually the representation can be concatenated.
means there is a function

concRepr :: repr -> repr -> repr

we can therefore define a *Combinator*, that takes one function of type (`src -> repr`) and returns one of type (`[src] -> repr`)

	combine :: (src -> repr) -> [src] -> repr
	combine showF listSrc =
		let listRepr = map showF listSrc in --first apply show on every src
		foldl concRepr emptyRepr listRepr

2.1 (Formatting) make something fill a certain space

this leads to using another type for the show

	renderF :: params -> src -> repr

shortcut type:

RenderFunction src repr params = params -> src -> repr

2.2. (Formatting) how to know about how much space some input needs minimum?

srcInfo :: src -> srcInfo

RenderMeth is a bundle of srcInfo and renderF




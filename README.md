Read at your own risk!








Project that serves as a project for school. Written in Haskell because Haskell is awesome, with the purpose of giving me a base to build a game out of. 

Woefully incomplete. Horrendously unperformant. 


Files:

Chars.hs contains the bulky mapping of characters to specific locations in the fontmap. Not a map I want in a file that will be read.
Loadmap.hs contains a few functions, used by Main, that use IO extensively. They load images to be inserted into the world. This seperates the IO from the more pure functions in Model; most of the IO functions in Model have IO only for debugging.
Model.hs contains the bulk of the program. It contains rendering code, including a typeclass Renderable a where render :: MonadReader W m => a -> m GreatImage (where GreatImage is the type for image data), code for the stepper function that steps the world every fraction of a second, and code for the event handler. These three components make up the `play` function of Gloss, my rendering engine.
Shaper defines functions that create shapes like rectangles and the borders around text boxes. Since my graphics are not advanced yet, it's a sparse file.
dun2.hs contains the main function, and it ties all the components in Model into `play`. 


Licensed under the MIT license I think, not that it matters. I would use the WTFPL but my teacher will look at this.

Requirements:
- FreeGLUT
- Haskell requirements:
	- gloss
	- gloss-juicy
	- htiled
	- lens
- Any remotely new GHC and cabal-install

Ideal installation flags: ghc dun2 -Odph -rtsopts -threaded -fno-liberate-case -funfolding-use-threshold1000 -funfolding-keeness-factor1000 -fllvm -optlo-O3
Leave out -fllvm if you're on Windows or otherwise don't have LLVM. 

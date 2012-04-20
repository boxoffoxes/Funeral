module Main where

import System( getArgs )

import HtmlGen.Syntax
import HtmlGen.Parser
import HtmlGen.Render


main :: IO ()
main = do
	args <- getArgs
	tree <- preProcess (Mac "import" $ head args)
	let lib = buildLibrary tree
	let tree' = stripDefs tree
	print $ render lib tree'



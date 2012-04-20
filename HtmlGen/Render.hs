module HtmlGen.Render where

import HtmlGen.Syntax
import HtmlGen.Parser

import Maybe
import List (unionBy)

type Renderer = Exp -> String


coreLibrary = []

buildLibrary :: Exp -> Library
buildLibrary e = parseDefs coreLibrary e

parseDef :: Exp -> Library
parseDef (Mul es) = concatMap parseDef es
parseDef (Def id e) = [ (id, e) ]
parseDef _ = []


parseDefs :: Library -> Exp -> Library
parseDefs l e = parseDef e


stripDefs :: Exp -> Exp
stripDefs (Mul es) = Mul es'
	where
		es' = map stripDefs $ filter notDef es
		notDef (Def _ _) = False
		notDef _  = True
stripDefs e = e


libraryLookup :: Library -> String -> Exp
libraryLookup l s = case lookup s l of 
	Just  e -> e
	Nothing -> error $ "Undefined tag '" ++ s ++ "'\n"

preProcess :: Exp -> IO Exp
preProcess (Mac id arg)
	| id == "insert"  = do 
		text <- readFile arg
		let e = Lit text
		return e
	| id == "import"  = do
		text <- readFile arg
		let e = Mul $ parse text
		preProcess e
preProcess (Mul es) = do 
		es' <- mapM preProcess es
		return $ Mul es'
preProcess e = do return e

{-
render :: Library -> Exp -> IO String
render _   (Lit s) = IO s
render _   (Mac id arg)
	| id == "insert"  = readFile arg
	| id == "import"  = 
-- render lib (Tag id attrs content) 
-- 	| id == "SELF" = tag (Tag id attrs content)
-- 	| otherwise    = map (render lib) 
render _ _ = IO ""
-}

renderAttrib :: Attr -> String
renderAttrib (attr, val) = " " ++ attr ++ "=\"" ++ val ++ "\""

-- renderTag :: Library -> Exp 

-- tag :: Exp -> String
-- tag (Tag id attrs content) = "<" ++ id ++ attrStr ++ ">" ++ concatMap (render lib) content ++ "</" ++ id ++ ">"
	-- where
		-- attrStr = concatMap renderAttrib attrs


mergeAttrs :: [Attr] -> [Attr] -> [Attr]
mergeAttrs defaults overrides = unionBy sameKey overrides defaults
	where
		sameKey (a, _) (b, _) = a == b



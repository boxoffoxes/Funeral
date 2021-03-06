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


render :: Library -> Exp -> String
render l (Lit s) = s
render l (Mul es) = concatMap (render l) es
render l (Tag id e) = renderTag l id e
render _ _ = ""


renderTag :: Library -> Id -> Exp -> String
renderTag l id e = 
	where
		e' = renderPrims $ libraryLookup l id
		renderPrims (Mul es) = Mul $ map renderPrims es
		renderPrims (Prim TagId) = Lit id
		renderPrims (Prim Attrs) = 
		renderPrims (Prim Content) = 


		( attrs, prims ) = decompose e
		attrStr = renderAttrs attrs
		content = render l e
		closure = case content of
			"" -> " />"
			_  -> ">" ++ content ++ "</" ++ id ++ ">"

renderAttrs :: [Attr] -> String
renderAttrs as = concatMap renderAttr as
	where
		renderAttr (attr, val) = " " ++ attr ++ "=\"" ++ (renderVal val) ++ "\""

		renderVal (Val s) = s
		renderVal (Ref r) = case (lookup r as) of
			Just (Val v)  -> v
			Just r'@(Ref _) -> renderVal r'
			Nothing       -> error $ "Reference to undefined attribute '" ++ r ++ "'\n"

findAttrs :: Exp -> [Attr]
findAttrs (Att a) = [a]
findAttrs (Mul es) = concatMap findAttrs es
findAttrs _ = []

-- findOpts :: Exp -> [Option]
-- findOpts (Opt s) -> [



{-
render _   (Lit s) = IO s
render _   (Mac id arg)
	| id == "insert"  = readFile arg
	| id == "import"  = 
-- render lib (Tag id attrs content) 
-- 	| id == "SELF" = tag (Tag id attrs content)
-- 	| otherwise    = map (render lib) 
render _ _ = IO ""
-}

-- renderTag :: Library -> Exp 

-- tag :: Exp -> String
-- tag (Tag id attrs content) = "<" ++ id ++ attrStr ++ ">" ++ concatMap (render lib) content ++ "</" ++ id ++ ">"
	-- where
		-- attrStr = concatMap renderAttrib attrs


mergeAttrs :: [Attr] -> [Attr] -> [Attr]
mergeAttrs defaults overrides = unionBy sameKey overrides defaults
	where
		sameKey (a, _) (b, _) = a == b



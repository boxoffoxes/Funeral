module HtmlGen.Parser where

import HtmlGen.ParseLib
import HtmlGen.Syntax

label :: Parser String
label = token $ atLeastOne alphanum


expr :: Parser Exp
expr =	pure Tag <*> label <*> keyword "[" |> attributes <*> maybeSome expr <| keyword "]"
	<|> pure Def <*> keyword "::" |> label <| keyword "=" <*> keyword "{" |> atLeastOne expr <| keyword "}"
	<|> pure Def <*> keyword "::" |> label <| keyword "=" <*> exactlyOne expr 
	<|> pure Mac <*> string ":" |> label <*> stringLiteral
	<|> pure Lit <*> stringLiteral
--	<|> pure Tag <*> label <*> pure [] <*> (exactlyOne $ pure Lit <*> stringLiteral)
--	<|> pure Tag <*> label <*> pure [] <*> bracketed $ pure Lit <*>


attributes :: Parser [Attr]
attributes = maybeSome (token attribute) <| maybeOne (keyword ";")

attribute :: Parser Attr
attribute = pure (,) <*> label <| keyword "=" <*> ( stringLiteral <|> label )


-- tag :: Parser Exp
-- tag =   pure Tag label <*> stringLiteral
--	<|> label <*> bracketed content

--content :: Parser String
-- content = attribs <*> keyword ";" <*> tagBody

--attribs :: Parser String
--attribs = string ""

-- tagBody :: Parser String
-- tagBody = string ""


parse :: String -> [Exp]
parse s = 
	where
		(junk, exp) = head $ concatMap snd $ maybeSome expr s



coreLibrary = []

buildLibrary :: [Exp] -> Library
buildLibrary es = map parseDef es ++ coreLibrary

parseDef :: Exp -> Definition
parseDef (Def id es) = (id, es)

render :: Library -> Exp -> String
render _   (Lit s) = s
render lib (Tag id attrs content) = "<" ++ id ++ attrStr ++ ">" ++ concatMap (render lib) content ++ "</" ++ id ++ ">"
	where
		attrStr = concatMap renderAttrib attrs
--		(_, exps) = lookup lib id
render _ _ = ""

renderAttrib :: Attr -> String
renderAttrib (attr, val) = " " ++ attr ++ "=\"" ++ val ++ "\""





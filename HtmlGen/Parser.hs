module HtmlGen.Parser where

import HtmlGen.ParseLib
import HtmlGen.Syntax

import List as L


token :: Parser a -> Parser a
token p = p <| optSpaces
--	where
--		ignoreChars = maybeSome $ satisfy isSpace <|> satisfy (== ",") <|> satisfy (== ";")

keyword :: String -> Parser String
keyword = token . string

{-escapeChar :: Parser String
escapeChar = string "\\\\" <|> string "\\\"" <|> string "\\'"

quotedRegion :: String -> String -> Parser String
quotedRegion open close = string open |> maybeSome <| string close
-}

stringLiteral :: Parser String
stringLiteral =   token ( char '"'  |> maybeSome ( satisfy (/= '"') )  <| char '"' )
              <|> token ( char '\'' |> maybeSome ( satisfy (/= '\'') ) <| char '\'' )

-- commentLiteral :: Parser String
-- commentLiteral =  token ( string "<!--"  |> maybeSome (  <| string "-->"


label :: Parser String
label = token $ atLeastOne symbolChar

reference :: Parser String
reference = char '#' |> label


expr :: Parser Exp
expr =	pure Tag <*> label <*> expr
	<|> pure Def <*> keyword "::" |> label <| keyword "=" <*> expr
	<|> pure Mac <*> string ":" |> label <*> stringLiteral
	<|> pure Lit <*> stringLiteral
	<|> pure Att <*> token attribute
	<|> pure Opt <*> string "?" |> label
	<|> pure Mul <*> keyword "[" |> maybeSome expr <| keyword "]"
--	<|> pure Com <*> keyword "<!--" |> 


attributes :: Parser [Attr]
attributes = maybeSome (token attribute) <| maybeOne (keyword ";")

value :: Parser Value
value = pure Ref <*> reference
	<|> pure Val <*> ( label <|> stringLiteral )

attribute :: Parser Attr
attribute = pure (,) <*> label <| keyword "=" <*> value


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
parse s = if junk /= "" then error ("Parse error at '" ++ take 30 junk ++ "...'\n") else exps
	where
		(junk, exps) = head $ maybeSome expr s



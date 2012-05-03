module HtmlReader where


import HtmlGen.ParseLib

data Expr = Tag Id [Attr] [Expr]
		  | Str String
	deriving Show

type Id String
type Attr = (Id, Expr)


symbol :: Parser Char
symbol = satisfy (`elem` "!%&|*+-/:?@^_~.#") 

digit :: Parser Char
digit = satisfy isDigit

letter :: Parser Char
letter = satisfy isAlpha


token :: Parser a -> Parser a
token p = p <| ignoredChars
    where
        ignoredChars = maybeSome $ satisfy (`elem` " \t\n\r;,")

keyword :: String -> Parser String
keyword = token . string


parseTag :: Parser Expr
parseTag = pure Tag <*> keyword "<" |> parseId <*> maybeSome parseAttrs <*> tagClose
	where
		tagClose = keyword "/>" |> pure []




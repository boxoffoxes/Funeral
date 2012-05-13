module Author.Parser where

import Char

import Author.Syntax


symbol :: Parser Char
symbol = satisfy (`elem` "!%&|*+-/:<>?@^_~.#") 

digit :: Parser Char
digit = satisfy isDigit

letter :: Parser Char
letter = satisfy isAlpha

padding :: Parser Char
padding = satisfy (`elem` "\n\r \t")


token :: Parser a -> Parser a
token p = p <| ignoredChars
    where
        ignoredChars = maybeSome $ satisfy (`elem` " \t;,")

keyword :: String -> Parser String
keyword = token . string


parseId :: Parser String
parseId = token $ pure (:) <*> (symbol <|> letter) <*> maybeSome (symbol <|> digit <|> letter)

parsePad :: Parser Exp
parsePad = token $ pure Str <*> atLeastOne padding

parseStr :: Parser Exp
parseStr = pure Str <*> s
    where 
        s =  token ( char '`'  |> maybeSome ( satisfy (/= '`')  ) <| char '`' )
         <|> token ( char '"'  |> maybeSome ( satisfy (/= '"')  ) <| char '"' )
         <|> token ( char '\'' |> maybeSome ( satisfy (/= '\'') ) <| char '\'' )

parseNum :: Parser Exp
parseNum = pure Num <*> n
    where
        n = token $ pure (read) <*> atLeastOne digit

parseApp :: Parser Exp
parseApp = pure App <*> parseId <| maybeSome parsePad <*> parseExpr

parseList :: Parser Exp
parseList = pure Lis <*> keyword "[" |> maybeSome parseExpr <| keyword "]"

parseAtt :: Parser Exp
parseAtt = pure Att <*> parseId <| keyword "=" <*> parseExpr -- <| maybeSome parsePad

parseRef :: Parser Exp
parseRef = pure Ref <*> string "$" |> ( parseId <|> keyword "0" <|> keyword "1" )

-- parseComment :: Parser Exp
-- parseComment = pure Str <*> keyword "<!--" |> text <| keyword "-->" )
	-- where
		-- text = maybeSome satisfy (

parseExpr :: Parser Exp
parseExpr = parsePad <|> parseApp <|> parseStr <|> parseNum <|> parseRef <|> parseAtt <|> parseList


parse :: String -> Exp
parse s = case junk of
    ""        -> Lis exps
    otherwise -> error ("Parse error in '" ++ take 30 junk ++ "...'\n")
    where
        (junk, exps) = head $ maybeSome parseExpr s



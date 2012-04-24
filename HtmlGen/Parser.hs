module HtmlGen.Parser where

import Char

import HtmlGen.ParseLib
import HtmlGen.Syntax

token :: Parser a -> Parser a
token p = p <| ignoredChars
	where
		ignoredChars = maybeSome $ satisfy (`elem` " \t\n\r;,")

keyword :: String -> Parser String
keyword = token . string

stringLiteral :: Parser String
stringLiteral =   token ( char '"'  |> maybeSome ( satisfy (/= '"') )  <| char '"' )
              <|> token ( char '\'' |> maybeSome ( satisfy (/= '\'') ) <| char '\'' )

symbolChar :: Parser Char
symbolChar = satisfy isSymChar
	where 
		isSymChar c = isAlphaNum c || c == '_'

symbol :: Parser String
symbol = token $ atLeastOne symbolChar


expr :: Parser Exp
expr =  pure App <*> symbol <*> expr
	<|> pure Lit <*> stringLiteral
	<|> pure Lis <*> keyword "[" |> maybeSome expr <| keyword "]"
	<|> pure Sym <*> keyword "#" |> symbol
	<|> pure Def <*> symbol <| keyword "=" <*> expr

parse :: String -> Exp
parse s = case junk of
	""        -> Lis exps
	otherwise -> error ("Parse error at '" ++ take 30 junk ++ "...'\n")
	where
		(junk, exps) = head $ maybeSome expr s

simplify :: Exp -> Exp
-- simplify ( App id e ) = simplify Lis [ Def "__id" id , Def "__content" e , ]
simplify (Lis []) = Lit ""
simplify (Lis [e]) = e
simplify (Lis es) = Lis $ map simplify $ mergeLiterals $ map simplify es
simplify e = e


mergeLiterals :: [Exp] -> [Exp]
mergeLiterals ( (Lit s1):(Lit s2):es ) = mergeLiterals ( (Lit (s1 ++ s2)):es )
mergeLiterals (e:es) = e:(mergeLiterals es)
mergeLiterals [] = []



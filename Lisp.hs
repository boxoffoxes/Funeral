module Lisp where

import HtmlGen.ParseLib

import System( getArgs )
import Char (isDigit, isAlpha, isAlphaNum)


data Expr	= Atom String
			| List [Expr]
			| Pair [Expr] Expr
			| Num Integer
			| Str String
			| Boo Bool
	deriving (Show, Eq)

safePunctuation = "!#$%&|*+-/:<=>?@^_~"

symbolFirstChar :: Parser Char
symbolFirstChar =   satisfy (`elem` safePunctuation) 
				<|> satisfy isAlpha

symbolChar :: Parser Char
symbolChar = satisfy (`elem` safePunctuation)
		 <|> satisfy isAlphaNum

digit :: Parser Char
digit = satisfy isDigit

token :: Parser a -> Parser a
token p = p <| ignoredChars
	where
		ignoredChars = maybeSome $ satisfy (`elem` " \t\n\r;,")

keyword :: String -> Parser String
keyword = token . string

stringLiteral :: Parser String
stringLiteral =   token ( char '"'  |> maybeSome ( satisfy (/= '"') )  <| char '"' )
--              <|> token ( char '\'' |> maybeSome ( satisfy (/= '\'') ) <| char '\'' )

symbol :: Parser String
symbol = token $ pure (:) <*> symbolFirstChar <*> maybeSome symbolChar

number :: Parser Integer
number = token $ pure (read) <*> atLeastOne digit

boolean :: Parser Bool
boolean = token $ pure read <*> ( keyword "True" <|> keyword "False" )
	
expr :: Parser Expr
expr =  pure Boo <*> boolean
	<|> pure Num <*> number
	<|> pure Str <*> stringLiteral
	<|> pure List <*> keyword "[" |> maybeSome expr <| keyword "]"
--	<|> pure Pair <*> atLeastOne expr <| keyword "." <*> expr <| keyword ")"
	<|> pure Atom <*> symbol

parse :: String -> [Expr]
parse s = case junk of
	""        -> exps
	otherwise -> error ("Parse error at '" ++ take 30 junk ++ "...'\n")
	where
		(junk, exps) = head $ maybeSome expr s

eval :: Expr -> Expr
eval ( List (Atom f:args) ) = apply f $ map eval args
eval e = e

apply :: String -> [Expr] -> Expr
apply f args = maybe (Boo False) ($ args) $ lookup f primitives

primitives :: [(String, [Expr] -> Expr)]
primitives = [	("+", numericBinop (+) ),
				("-", numericBinop (-) ),
				("*", numericBinop (*) ),
				("/", numericBinop div ),
				("mod", numericBinop mod ),
				("quotient", numericBinop quot ),
				("remainder", numericBinop rem ),
				("concat", concatStr) ]

concatStr :: [Expr] -> Expr
concatStr es = Str $ concat $ map show es
	where
		stringVal :: [Expr] -> String
		stringVal (Num n) = show n
		stringVal (Boo b) = show b
		stringVal (Str s) = s
		stringVal _ = ""


numericBinop :: (Integer -> Integer -> Integer) -> [Expr] -> Expr
numericBinop op params = Num $ foldl1 op $ map unpackNum params		

unpackNum :: Expr -> Integer
unpackNum (Num n) = n
unpackNum _ = 0


main :: IO ()
main = getArgs >>= print . eval . head . parse . head
--	putStrLn $ head . args !! 0
	-- putStrLn $ parse " 


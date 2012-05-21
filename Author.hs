module Main where

import System( getArgs )
import Char
import IO (readFile)
import List (nubBy, partition, intersperse)

import Funeral.ParseLib

data Exp = App Id Exp
         | Num Integer -- 1
         | Str String  -- "string" or 'string' or `string`
         | Att Id Exp  -- key=value
         | Lis [Exp]   -- [ e1 e2 e3 ]
         | Com String  -- -- comment

instance Show Exp where 
    show (App id e) = id ++ " {" ++ show e ++ "}"
    show (Num n) = show n
    show (Str s) = show s
    show (Att id e) = id ++ "=" ++ show e
    show (Lis es) = concat ( intersperse ";" (map show es) )
    show (Com s) = show $ "<!-- " ++ s ++ " -->"

type Id = String

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

--parseRef :: Parser Exp
--parseRef = pure Ref <*> string "$" |> ( parseId <|> keyword "0" <|> keyword "1" )

parseComment :: Parser Exp
parseComment = token $ pure Com <*> keyword "--" |> ( maybeSome $ anyCharExcept "\n\r" ) <| ( maybeSome $ satisfy (`elem` "\n\r") )

parseExpr :: Parser Exp
parseExpr = parseComment <|> parsePad <|> parseApp <|> parseStr <|> parseNum <|> parseAtt <|> parseList


parse :: String -> Exp
parse s = case junk of
    ""        -> Lis exps
    otherwise -> error ("Parse error in '" ++ take 30 junk ++ "...'\n")
    where
        (junk, exps) = head $ maybeSome parseExpr s



main :: IO ()
main = do
    args <- getArgs
    sources <- mapM readFile args
    let tree = parse $ concat sources
    -- let lib = buildLibrary tree
    -- let tree' = stripDefs tree
    putStrLn "require 'author.formlib'\n"
    putStrLn $ show tree


module Main where

import System( getArgs )
import Char
import List (nubBy)

import HtmlGen.ParseLib

data Exp = Tag Id Exp  -- fun arg
         | Def Id Exp  -- :: name = [ ... ]
         | Num Integer -- 1
         | Str String  -- "string" or 'string' or `string`
         | Att Id Exp  -- key=value
         | Ref Id      -- $key
         | Lis [Exp]   -- [ e1 e2 e3 ]
    deriving Show

type Id = String
type Library = [ Definition ]
type Definition = ( Id, Exp )


symbol :: Parser Char
symbol = satisfy (`elem` "!%&|*+-/:<>?@^_~") 

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


parseId :: Parser String
parseId = token $ pure (:) <*> (symbol <|> letter) <*> maybeSome (symbol <|> digit <|> letter)

{- parseLongId :: Parser (String, [Exp])
parseLongId = pure (,) <*> id <*> es
    where
        id = parseId
        es = maybeSome ( pure Att <*> "id" <*> char '#' |> parseId <|> pure Att ) -}

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
parseApp = pure App <*> parseId <*> parseExpr

parseList :: Parser Exp
parseList = pure Lis <*> keyword "[" |> maybeSome parseExpr <| keyword "]"

parseAtt :: Parser Exp
parseAtt = pure Att <*> parseId <| keyword "=" <*> parseExpr

parseRef :: Parser Exp
parseRef = pure Ref <*> string "$" |> ( parseId <|> keyword "0" <|> keyword "1" )

parseDef :: Parser Exp
parseDef = pure Def <*> keyword "#def" |> parseId <| keyword "=" <*> parseExpr

parseExpr :: Parser Exp
parseExpr = parseDef <|> parseApp <|> parseStr <|> parseNum <|> parseRef <|> parseAtt <|> parseList


parse :: String -> Exp
parse s = case junk of
    ""        -> Lis exps
    otherwise -> error ("Parse error at '" ++ take 30 junk ++ "...'\n")
    where
        (junk, exps) = head $ maybeSome parseExpr s


coreLibrary = []

buildLibrary :: Exp -> Library
buildLibrary e = getDefs coreLibrary e

getDefs :: Library -> Exp -> Library
getDefs l e = getDef e ++ l

getDef :: Exp -> Library
getDef (Lis es) = reverse $ concatMap getDef es
getDef (Def id e) = [ (id, e) ]
getDef _ = []

stripDefs :: Exp -> Exp
stripDefs (Lis es) = Lis es'
	where
		es' = map stripDefs $ filter notDef es
		notDef (Def _ _) = False
		notDef _  = True
stripDefs e = e




eval :: Library -> Library -> Exp -> Exp
eval l sc e@(App f arg) = eval l sc' e'
    where
        sc' = tidyLibrary $ createScope sc $ App f $ eval l sc arg
        e' =  libraryLookup l f
eval l sc (Lis []) = Str ""
eval l sc (Lis [e]) = eval l sc e
eval l sc (Lis es) = Lis $ map (eval l sc) $ mergeLiterals $ map (eval l sc) es
eval l sc (Att id e) = Att id $ eval l sc e
eval l sc (Ref r) = val
    where
        val = case lookup r sc of
            Just v  -> v
            Nothing -> error $ "undefined reference to " ++ r
eval l sc e = e

libraryLookup :: Library -> String -> Exp
libraryLookup l id = case lookup id l of 
	Just  e -> e
	Nothing -> error $ "Undefined tag '" ++ id ++ "'\n"

createScope :: Library -> Exp -> Library
createScope sc (App id (Lis es)) = ("0", Str id):("1", Lis es):getAttrs es ++ sc
createScope sc (App id a@(Att k v)) = ("0", Str id):("1", Lis [a] ):(k, v):[] ++ sc
createScope sc _ = []

getAttrs :: [Exp] -> Library
getAttrs es = map unpackAtt $ filter (isAtt) es
    where
        isAtt (Att _ _) = True
        isAtt _ = False
        unpackAtt (Att k v) = (k, v)

tidyLibrary :: Library -> Library
tidyLibrary = nubBy (\a b -> fst a == fst b)



mergeLiterals :: [Exp] -> [Exp]
mergeLiterals ( (Str s1):(Str s2):es ) = mergeLiterals ( (Str (s1 ++ s2)):es )
mergeLiterals (e:es) = e:(mergeLiterals es)
mergeLiterals [] = []


main :: IO ()
main = do
    args <- getArgs
    let tree = parse $ head args
    let lib = buildLibrary tree
    let tree' = stripDefs tree
    print $ eval lib [] tree'


module Main where

import System( getArgs )
import Char
import IO (readFile)
import List (nubBy, partition, intersperse)

import HtmlGen.ParseLib

data Exp = Tag Id [Definition] Exp
         | App Id Exp  -- fun arg
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
symbol = satisfy (`elem` "!%&|*+-/:<>?@^_~.#") 

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

partitionAttrs :: Exp -> ([Definition], Exp)
partitionAttrs (Lis es) = (map unpackAttr as, Lis content)
    where
        (as, content) = partition isAttr es
        isAttr (Att _ _) = True
        isAttr _ = False
partitionAttrs a@(Att _ _) = ([unpackAttr a], Lis [])
partitionAttrs e = ([], e)

unpackAttr :: Exp -> Definition
unpackAttr (Att k v) = (k, v)

eval :: Library -> Library -> Exp -> Exp
eval l sc (App id arg) = case lookup id l of 
    Just e -> eval l sc' e
    Nothing -> Tag id as content
    where
        (as, content) = partitionAttrs $ eval l sc arg
        sc' = tidyLibrary $ createScope sc $ App id $ eval l sc arg
{- eval l sc e@(App f arg) = eval l sc' e'
    where
        e' =  libraryLookup l f  -}
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
getAttrs es = fst $ partitionAttrs $ Lis es

tidyLibrary :: Library -> Library
tidyLibrary = nubBy (\a b -> fst a == fst b)



mergeLiterals :: [Exp] -> [Exp]
mergeLiterals ( (Str s1):(Str s2):es ) = mergeLiterals ( (Str (s1 ++ s2)):es )
mergeLiterals (e:es) = e:(mergeLiterals es)
mergeLiterals [] = []


render :: Exp -> String
render (Str s) = s
render (Num n) = show n
render (Tag id as content) = openTag ++ body ++ closeTag
    where
        body = render content
        (openTag, closeTag) = case body of
            ""        -> ( '<' : id ++ renderAttrs as ++ " />", "")
            otherwise -> ( '<' : id ++ renderAttrs as ++ ">",   "</" ++ id ++ ">")
render (Lis es) = concat $ intersperse " " $ map render es
render e = error $ "Cannot render " ++ show e

renderAttrs :: [Definition] -> String
renderAttrs as = case null as of
    True -> ""
    otherwise -> ' ' : ( concat $ intersperse " " $ map renderAttr as )
    where
        renderAttr (k, v) = k ++ "=\"" ++ render v ++ "\""


main :: IO ()
main = do
    args <- getArgs
    sources <- mapM readFile args
    let tree = parse $ concat sources
    let lib = buildLibrary tree
    let tree' = stripDefs tree
    putStrLn $ render $ eval lib [] tree'


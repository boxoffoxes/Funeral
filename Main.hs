module Main where

import System( getArgs )
import Char
import IO (readFile)
import List (nubBy, partition, intersperse)

import Author.ParseLib

data Exp = App Id Exp  -- fun arg
         | Num Integer -- 1
         | Str String  -- "string" or 'string' or `string`
         | Att Id Exp  -- key=value
         | Ref Id      -- $key
         | Lis [Exp]   -- [ e1 e2 e3 ]
		 | Fun (Exp -> Exp)
--    deriving Show

instance Show Exp where 
	show (App id exp) = "App " ++ show id ++ " " ++ show exp
	show (Num n) = "Num " ++ show n
	show (Str s) = "Str " ++ show s
	show (Att id e) = "Att " ++ show id ++ " " ++ show e
	show (Lis es) = "Lis " ++ show es
	show (Ref r) = "Ref " ++ show r
	show (Fun f) = "<function>"
	

type Id = String
type Library = [ Definition ]
type Definition = ( Id, Exp )



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


-- if each top-level declaration stores its own scope, 
-- new declarations can maybe extend older ones, forth-style;
coreLibrary = [ 
				("!tag", Fun fnTag), ("!emptyTag", Fun fnEmptyTag) ]

buildLibrary :: Exp -> Library
buildLibrary e = getDefs coreLibrary e

getDefs :: Library -> Exp -> Library
getDefs l e = getDef e ++ l

getDef :: Exp -> Library
getDef (Lis es) = reverse $ concatMap getDef es
getDef (Att id e) = [ (id, e) ]
getDef _ = []

stripDefs :: Exp -> Exp
stripDefs (Lis es) = Lis es'
	where
		es' = map stripDefs $ filter notDef es
		notDef (Att _ _) = False
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

eval :: (Library, Exp) -> (Library, Exp)
eval (l, App id arg) = case lookup id l of
    Just (Fun f) -> (l, f arg)
    Just x       -> error $ "Can't apply a non-function value: " ++ show x
    Nothing      -> error $ "Undefined function '" ++ id ++ "'"
eval (l, Lis [])    = ( l,  Str "")
eval (l, Lis [e])   = eval (l, e)
eval (l, Lis es)    = (l, Lis $ evalList l es)
eval (l, Att id e)  = ( l', Att id e' ) -- not sure about this. infinite loopy goodness?
	where
		l' = (id, e') : l
		e' = snd $ eval (l, e)
eval (l, Ref r)     = case lookup r l of
	Just v    -> eval (l, v)
	Nothing   -> (l, Ref r)
eval (l, e)         = (l, e)

evalList :: Library -> [Exp] -> [Exp]
evalList l [] = []
evalList l (e:es) = e': evalList l' es 
    where
        (l', e') = eval (l, e)

{-
eval ( l, (App id arg) ) = case lookup id l of 
    Just (Fun f) -> (f arg) l -- needs eval?
    Nothing -> error $ "Undefined function '" ++ id ++ "'" -- Tag id as content
eval l (Lis []) = (Str "") l
eval l (Lis [e]) = eval l e
eval l (Lis es) = Lis $ map (eval l) $ mergeLiterals $ map (eval l) es
eval l (Att id e) = (Att id $ eval l e) l'
eval l (Ref r) = val
    where
        val = case lookup r l of
            Just v  -> eval l v
            Nothing -> Ref r
eval l e = e
-}
-- libraryLookup :: Library -> String -> Exp
-- libraryLookup l id = case lookup id l of 
	-- Just  e -> e
	-- Nothing -> error $ "Undefined tag '" ++ id ++ "'\n"

fnTag :: Exp -> Exp
fnTag (Str s) = Fun $ makeTag False s

fnEmptyTag :: Exp -> Exp
fnEmptyTag (Str s) = Fun $ makeTag True s

makeTag :: Bool -> String -> Exp -> Exp
makeTag empty id arg = Str $ ('<':id) ++ attrStr ++ closure
	where
		(as, c) = partitionAttrs arg
		attrStr = case as of
			[] -> ""
			_  -> renderAttrs as
		content = render c
		closure = case (content, empty) of
			("", True)  -> " />"
			("", False) -> "></" ++ id ++ ">"
			(e,  _)     -> ">" ++ content ++ "</" ++ id ++ ">"


	


mergeLiterals :: [Exp] -> [Exp]
mergeLiterals ( (Str s1):(Str s2):es ) = mergeLiterals ( (Str (s1 ++ s2)):es )
mergeLiterals (e:es) = e:(mergeLiterals es)
mergeLiterals [] = []


render :: Exp -> String
render (Str s) = s
render (Num n) = show n
-- render (Tag ('<':id) as content) = renderTag id as content
-- render (Tag id as content) = renderTag id as content
render (Lis es) = concat $ {- intersperse " " $ -} map render es
render (Att _ _) = ""
render (Ref r) = error $ "Found an undefined reference to " ++ show r
render e = error $ "Cannot render " ++ show e

-- renderTag :: Id -> [Definition] -> Exp -> String
-- renderTag id as content = openTag ++ body ++ closeTag
    -- where
        -- body = render content
        -- (openTag, closeTag) = ( '<' : id ++ renderAttrs as ++ ">",   "</" ++ id ++ ">")
--        (openTag, closeTag) = case body of
--            ""        -> ( '<' : id ++ renderAttrs as ++ " />", "")
--            otherwise -> ( '<' : id ++ renderAttrs as ++ ">",   "</" ++ id ++ ">")

renderAttrs :: [Definition] -> String
renderAttrs as = case null as of
    True -> ""
    otherwise -> ' ' : ( concat $ intersperse " " $ map renderAttr as )
    where
        renderAttr (k, v) = k ++ "=\"" ++ render v ++ "\""


main :: IO ()
main = do
    args <- getArgs
    -- sources <- mapM readFile args
    let tree = parse $ concat args --sources
    -- let lib = buildLibrary tree
    -- let tree' = stripDefs tree
    putStrLn $ render $ snd $ eval (coreLibrary, tree)


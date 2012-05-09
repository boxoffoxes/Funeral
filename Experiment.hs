module Main where

import System( getArgs )
import Char
import IO (readFile)
import List (nubBy, partition, intersperse, find)

import Author.ParseLib

data Expr = Word Id
          | List [Expr]
		  | Bool Bool
	      | Str String
		  | Num Integer
		  | Fun (State -> State)

instance Show Expr where
	show (Word id) = '\'' : id
	show (Num n) = show n
	show (Bool b) = '?' : show b
	show (Str s) = show s
	show (List es) = "[" ++ (concat $ intersperse " " $ map show es) ++ "]"
	show (Fun _) = "<function>"


data Defn = Defn {
	name :: Id,
	body :: Expr}
		deriving Show


type Id = String
type Library = [ Defn ]
type Stack = [ Expr ]
type State = (Library, Stack)


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


parseString :: Parser Expr
parseString = pure Str <*> s
    where 
        s =  token ( char '`'  |> maybeSome ( satisfy (/= '`')  ) <| char '`' )
         <|> token ( char '"'  |> maybeSome ( satisfy (/= '"')  ) <| char '"' )
         <|> token ( char '\'' |> maybeSome ( satisfy (/= '\'') ) <| char '\'' )

parseNumber :: Parser Expr
parseNumber = pure Num <*> n
	where
		n = token $ pure (read) <*> atLeastOne digit

parseId :: Parser String
parseId = token $ atLeastOne ( satisfy (not . (elem' " \t\n\r[]\"'`") ) )
	where
		elem' a b = elem b a

parseBool :: Parser Expr
parseBool = pure Bool <*> ( pure read <*> ( keyword "True" <|> keyword "False" ) )

parseWord :: Parser Expr
parseWord = pure Word <*> parseId

parseList :: Parser Expr
parseList = pure List <*> keyword "[" |> maybeSome parseExpr <| keyword "]"

parseExpr :: Parser Expr
parseExpr = parseBool <|> parseNumber <|> parseList <|> parseString <|> parseWord

parse :: String -> [Expr]
parse s = case junk of
    ""        -> exps
    otherwise -> error ("Parse error in '" ++ take 30 junk ++ "...'\n")
    where
        (junk, exps) = head $ maybeSome parseExpr s


primDef :: State -> State
primDef (l, Word id:e:st') = (l', st')
	where
		l' = (Defn id e) : l


primHead :: State -> State
primHead (l, (List (e:es) : st')) = (l, e:st')
primHead _ = error "Can't get the first item of a non-list"

primTail :: State -> State
primTail (l, (List (e:es) : st')) = (l, List es : st')

primConcat :: State -> State
primConcat (l, List es:st') = (l, Str (concat $ map show es) : st')

primType :: State -> State
primType (l, st@(List _:_)) = (l, Str "list":st)
primType (l, st@(Word _:_)) = (l, Str "word":st)
primType (l, st@(Bool _:_)) = (l, Str "boolean":st)
primType (l, st@(Str _:_))  = (l, Str "string":st)
primType (l, st@(Fun _:_))  = (l, Str "function":st)
primType (l, st@(Num _:_))  = (l, Str "number":st)

primNot :: State -> State
primNot (l, Bool b:st) = (l, Bool (not b):st)


primEq :: State -> State
primEq ( l, Word a : Word b : st ) = (l, Bool (a == b) : st)
primEq ( l, Bool a : Bool b : st ) = (l, Bool (a == b) : st)
primEq ( l, Num  a : Num  b : st ) = (l, Bool (a == b) : st)
primEq ( l, Str  a : Str  b : st ) = (l, Bool (a == b) : st)
primEq ( l, a      : b      : st ) = (l, Bool False : st)

{- primDip :: State -> State
primDip (l, Fun f : e : es ) = (l', e:es')
	where
		(l', es') = f (l, es) -}

primDrop :: State -> State
primDrop (l, _:st) = (l, st)

primSwap :: State -> State
primSwap (l, a:b:st) = (l, b:a:st)

primRot :: State -> State
primRot (l, a:b:c:st) = (l, c:a:b:st)

primDup :: State -> State
primDup (l, a:st) = (l, a:a:st)

{-primMap :: State -> State
primMap (l, Fun f:List es:st) = (l, List es':st)
	where
		es' = map f es

primFold :: State -> State
primFold (l, Fun f:List es:e:st) = (l, e':st)
	where
		e' = foldr f es
-}

transform :: State -> State
transform (l, Word w:st) = case find (\(Defn id f) -> id == w) l of
	Just (Defn _ e) -> transform (l, e:st)
	Nothing -> (l, Word w : st) -- eval st first?
transform (l, Fun f:Fun g:st) = transform (l, Fun (f . g):st)
transform (l, List [e]:st) = transform (l, e:st)
transform (l, List es:st) = transform (l, es)
transform (l, e:es) = (l', e:es')
	where
		(l', es') = transform (l, es)
transform (l, []) = (l, [])

eval :: State -> State
eval (l, Word w:st) = case find (\(Defn id f) -> id == w) l of
	Just d  -> case body d of 
		List es -> eval (l, es ++ st)
		e       -> eval (l, e : st)
	Nothing -> (l, Word w : st)
eval (l, Fun f : Fun g : st) = (l, Fun (f . g) : st)
eval (l, Fun f : st) = eval $ f $ eval (l, st)
-- eval (l, List [] :st) = eval (l, st)
-- eval (l, List [e]:st) = eval (l, e:st)
-- eval (l, List es:st) = eval (l, es ++ st)
eval (l, e:es) = (l', e:es')
	where
		(l', es') = eval (l, es)
eval (l, []) = (l, [])



numericBinaryPrim :: (Integer -> Integer -> Integer) -> State -> State
numericBinaryPrim f (l, (Num x  : Num y  : st')) = (l, Num  (f x y) : st')

booleanBinaryPrim :: (Bool -> Bool -> Bool) -> State -> State
booleanBinaryPrim f (l, (Bool x : Bool y : st')) = (l, Bool (f x y) : st')

prims = [
	Defn "+" (Fun $ numericBinaryPrim (+)),
	Defn "-" (Fun $ numericBinaryPrim (-)),
	Defn "*" (Fun $ numericBinaryPrim (*)),
	Defn "/" (Fun $ numericBinaryPrim div),
	Defn "%" (Fun $ numericBinaryPrim mod),

	Defn "&&" (Fun $ booleanBinaryPrim (&&)),
	Defn "||" (Fun $ booleanBinaryPrim (||)),
	Defn "not" (Fun primNot),
	Defn "=" (Fun primEq),
	
	Defn "swap" (Fun primSwap),
	--Defn "dip" (Fun primDip),
	Defn "drop" (Fun primDrop),
	Defn "rot" (Fun primRot),
	Defn "dup" (Fun primDup),

	Defn "concat" (Fun primConcat),
	Defn "head" (Fun primHead),
	Defn "tail" (Fun primTail),
	Defn "type" (Fun primType),

	Defn "def" (Fun primDef) ]



main :: IO ()
main = do
    args <- getArgs
    -- sources <- mapM readFile args
    let stack = parse $ concat args --sources
    let lib = prims
    putStrLn $ show $ transform (lib, stack)


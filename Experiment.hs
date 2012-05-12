module Main where

import System( getArgs )
import Char
import IO (readFile)
import List (nubBy, partition, intersperse, find)

import Debug.Trace

import Author.ParseLib

data Expr = Word Id
          | Quot [Expr]
          | Bool Bool
          | Pair Expr Expr
          -- | Str String
          | Chr Char
          | Num Int
          | Fun (State -> State)
          | Comm String

instance Eq Expr where
    Word w == Word x    =    w == x
    Num  n == Num  m    =    n == m
    Bool b == Bool c    =    b == c
    Chr  c == Chr  d    =    c == d
    Quot es == Quot xs  =    es == xs
    Pair e f == Pair x y =   e == x && x == y
    Fun  _ == Fun _     =    error "Cannot compare functions"
    _      == _         =    False
    

instance Show Expr where
    show (Word id) = id
    show (Num n) = show n
    show (Bool b) = '?' : show b
    show (Pair a b) = "(" ++ show a ++ " " ++ show b ++ ")"
    show (Chr c) = '.':c:[]
    show (Quot es) = "[" ++ (concat $ intersperse " " $ map show es) ++ "]"
    show (Fun _) = "<function>"
    show (Comm s) = "" -- "-- " ++ s ++ "\n"


data Defn = Defn {
        name :: Id,
        body :: [Expr]
    } deriving Show


type Id = String
type Library = [ Defn ]
type Stack = [ Expr ]
type State = (Library, Stack)


digit :: Parser Char
digit = satisfy isDigit

letter :: Parser Char
letter = satisfy isAlpha

whiteSpace :: Parser Char
whiteSpace = satisfy (`elem` "\n\r \t")

strToQuote :: String -> Expr
strToQuote s = Quot (map Chr s)

token :: Parser a -> Parser a
token p = p <| ignoredChars
    where
        ignoredChars = maybeSome $ whiteSpace

keyword :: String -> Parser String
keyword = token . string

parseChar :: Parser Expr
parseChar = token $ pure Chr <*> char '.' |> anyChar

parseString :: Parser Expr
parseString = pure strToQuote <*> s
    where 
        s =  token ( char '`'  |> maybeSome ( satisfy (/= '`')  ) <| char '`' )
         <|> token ( char '"'  |> maybeSome ( satisfy (/= '"')  ) <| char '"' )
         <|> token ( char '\'' |> maybeSome ( satisfy (/= '\'') ) <| char '\'' )

parseNumber :: Parser Expr
parseNumber = pure Num <*> n
    where
        n = token $ pure (read) <*> atLeastOne digit

parseComment :: Parser Expr
parseComment = token $ pure Comm <*> keyword "--" |> ( maybeSome $ anyCharExcept "\n\r" ) <| ( maybeSome $ satisfy (`elem` "\n\r") )

parsePair :: Parser Expr
parsePair = pure Pair <*> keyword "(" |> parseExpr <*> parseExpr <| keyword ")"

parseId :: Parser String
parseId = token $ pure (:) <*> anyCharExcept reservedPrefixes <*> ( maybeSome $ anyCharExcept reservedChars )
    where
        reservedPrefixes = ".0123456789" ++ reservedChars
        reservedChars = " \t\n\r\0[]\"'`()"

parseBool :: Parser Expr
parseBool = pure Bool <*> ( pure read <*> ( keyword "True" <|> keyword "False" ) )

parseWord :: Parser Expr
parseWord = pure Word <*> parseId

parseQuot :: Parser Expr
parseQuot = token $ pure Quot <*> keyword "[" |> maybeSome parseExpr <| keyword "]"

parseExpr :: Parser Expr
parseExpr = parseComment <|> parseBool <|> parseChar <|> parseNumber <|> parseQuot <|> parsePair <|> parseString <|> parseWord

parse :: String -> [Expr]
parse s = case junk of
    ""        -> exps
    otherwise -> error ("Parse error in '" ++ take 30 junk ++ "...'\n")
    where
        (junk, exps) = head $ maybeSome parseExpr s

---------------------------------------------------------------------
-- Primitive functions
---------------------------------------------------------------------

-- Stack -> Stack

fnNot :: Stack -> Stack
fnNot  (Bool False:st) = Bool True:st
fnNot  (e:st) = Bool False:st

fnOr :: Stack -> Stack
fnOr (y:x:st) = case truthValueOf x of
    True  -> x:st
    False -> y:st
        
fnAnd :: Stack -> Stack
fnAnd (y:x:st) = case truthValueOf x of
    False -> x:st
    True  -> y:st

fnNull :: Stack -> Stack
fnNull (Quot []:st) = Bool True:st
fnNull (e:st) = Bool False:st

truthValueOf :: Expr -> Bool
truthValueOf (Bool False) = False
truthValueOf e = True

fnType :: Stack -> Stack
fnType st@(Quot _:_)   = strToQuote "quotation":st
fnType st@(Word _:_)   = strToQuote "word":st
fnType st@(Bool _:_)   = strToQuote "boolean":st
fnType st@(Chr _:_)    = strToQuote "character":st
fnType st@(Fun _:_)    = strToQuote "function":st
fnType st@(Num _:_)    = strToQuote "number":st
fnType st@(Pair _ _:_) = strToQuote "number":st

-- fnPutChar :: Stack -> Stack
-- fnPutChar (Chr c:st) = st

fnSplitAt (Num n:Quot es:st) = Quot as:Quot bs:st
    where
        (as, bs) = splitAt n es

-- fnFoldr (Quot fn:e:Quot es:st) = 

stackFunctions = [
    ( "drop", \(e:st) -> st ),
    ( "swap", \(e:f:st) -> f:e:st ),
    ( "rot",  \(e:f:g:st) -> g:e:f:st ),
    ( "dup",  \(e:st) -> e:e:st ),

    ( "=",    \(e:f:st) -> Bool (e == f):st ),
    ( "not",  fnNot ),
    ( "or",   fnOr ),
    ( "and",  fnAnd ),

    ( "quote",   \(e:st) -> Quot [e]:st ),
    ( "compose", \(Fun f:Fun g:st) -> Fun (f . g):st ),
    ( "cons",    \(e:Quot es:st) -> Quot (e:es):st ),
    ( "uncons",  \(Quot (e:es):st) -> e:Quot es:st ),
    ( "splitAt", fnSplitAt ),

    ( "type",    fnType ) ]

-- State -> State functions

primDef :: State -> State
primDef (l, Word id:Quot es:st') = (l', st')
    where
        l' = (Defn id es) : l

primDip :: State -> State
primDip (l, Quot q : e : es ) = (l', e:es')
    where
        (l', es') = primApply (l, Quot q:es)

primApply :: State -> State
primApply (l, Fun f:st)     = descend $ f (l, st)
primApply (l, Quot es:st)   = descend (l, es ++ st)
primApply (_, e:st)         = error $ "Don't know how to apply " ++ show e
-- primApply (l, st)           = descend (l, st)

primEval :: State -> State -- do this depth-first!
primEval (l, [])         = (l, [])
primEval (l, Word w:st)  = case find (\(Defn id f) -> id == w) l of
                                Nothing -> (l, Word w:st)
                                Just d  -> descend (l, (body d) ++ st)
primEval t@(l, Fun f:st) = primApply t -- f (l, st)
primEval (l, Comm _:st)  = (l, st)
primEval (l, e:st)       = (l, e:st)

descend :: State -> State
descend (l, []) = (l, [])
descend (l, e:st) = primEval (l', e:st')
    where
        (l', st') = descend (l, st)

numericBinaryPrim :: (Int -> Int -> Int) -> State -> State
numericBinaryPrim f (l, (Num x  : Num y  : st')) = (l, Num  (f x y) : st')
numericBinaryPrim f (l, st) = (l, Fun (numericBinaryPrim f):st)

-- wrapper for functions that don't manipulate the library

makeStateful :: (Stack -> Stack) -> State -> State
makeStateful f (l, st) = (l, f st)

makeDefn :: (String, Stack -> Stack) -> Defn
makeDefn (id, f) = Defn id [Fun $ makeStateful f]

prims = (map makeDefn stackFunctions) ++ [
    Defn "dip" [Fun primDip],

    Defn "+" [Fun $ numericBinaryPrim (+)],
    Defn "-" [Fun $ numericBinaryPrim (-)],
    Defn "*" [Fun $ numericBinaryPrim (*)],
    Defn "/" [Fun $ numericBinaryPrim div],
    Defn "%" [Fun $ numericBinaryPrim mod],

    Defn "def" [Fun primDef] ]




---------------------------------------------------------------------
-- Argument processing and main
---------------------------------------------------------------------

main :: IO ()
main = do
    args <- getArgs
    sources <- mapM readFile (args ++ ["headstone.fn"])
    let stack = parse $ concat sources --args
    let lib = prims
    putStrLn $ show $ descend (lib, trace (show stack) stack)


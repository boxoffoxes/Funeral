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
    show (Quot es) = case all typeIsChar es of
            True  -> "\"" ++ ( map (\(Chr c) -> c) es ) ++ "\""
            False -> "[" ++ (concat $ intersperse " " $ map show es) ++ "]"
        where
            typeIsChar (Chr _) = True
            typeIsChar _       = False
    show (Fun _) = "<function>"
    show (Comm s) = "" -- "-- " ++ s ++ "\n"


data Defn = Defn {
        name :: Id,
        body :: [Expr]
    } deriving Show


type Id = String
type Library = [ Defn ]
type Prog = [ Expr ]
type State = (Library, Prog)


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

-- Prog -> Prog

fnNot :: Prog -> Prog
fnNot  (Bool False:st) = Bool True:st
fnNot  (e:st) = Bool False:st

fnOr :: Prog -> Prog
fnOr (y:x:st) = case truthValueOf x of
    True  -> x:st
    False -> y:st
        
fnAnd :: Prog -> Prog
fnAnd (y:x:st) = case truthValueOf x of
    False -> x:st
    True  -> y:st

fnNull :: Prog -> Prog
fnNull (Quot []:st) = Bool True:st
fnNull (Quot q:st)  = Bool False:st
fnNull (e:st) = barf st ("Cannot determine if non-quote value '" ++ show e ++ "' is null.")

truthValueOf :: Expr -> Bool
truthValueOf (Bool False) = False
truthValueOf e = True

fnType :: Prog -> Prog
fnType st@(Quot _:_)   = strToQuote "quotation":st
fnType st@(Word _:_)   = strToQuote "word":st
fnType st@(Bool _:_)   = strToQuote "boolean":st
fnType st@(Chr _:_)    = strToQuote "character":st
fnType st@(Fun _:_)    = strToQuote "function":st
fnType st@(Num _:_)    = strToQuote "number":st
fnType st@(Pair _ _:_) = strToQuote "number":st

-- fnPutChar :: Prog -> Prog
-- fnPutChar (Chr c:st) = st


fnSplitAt (Num n:Quot es:st) = Quot as:Quot bs:st
    where
        (as, bs) = splitAt n es

-- fnFoldr (Quot fn:e:Quot es:st) = 

progFunctions = [
    ( "drop", \(e:st) -> st ),
    ( "swap", \(e:f:st) -> f:e:st ),
    ( "rot",  \(e:f:g:st) -> g:e:f:st ),
    ( "unrot",  \(e:f:g:st) -> f:g:e:st ),
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
    ( "null",    fnNull ),
    ( "append",  \(Quot b:Quot a:st) -> Quot (a ++ b):st ),

    ( "show",    \(e:st) -> (strToQuote $ show e):st ),
    ( "type",    fnType ) ]

-- State -> State functions

primDef :: State -> State
primDef (l, Word id:Quot es:st') = (l', st')
    where
        l' = (Defn id es) : l

primDip :: State -> State
primDip (l, Quot q : e : st ) = (l', e:st')
    where
        (l', st') = primApply (l, Quot q:st)

primDig :: State -> State
primDig (l, Num n:Quot q:st) = (l', es ++ st')
    where
        es = take n st
        (l', st') = primApply (l, Quot q:(drop n st))

primApply :: State -> State
primApply (l, Fun f:st)     = descend $ f (l, st)
primApply (l, Quot es:st)   = descend (l, es ++ st)
primApply (_, e:st)         = barf st $ "Don't know how to apply " ++ show e

primEval :: State -> State -- do this depth-first!
primEval (l, [])         = (l, [])
primEval (l, Word w:st)  = case find (\(Defn id f) -> id == w) l of
                                Nothing -> (l, Word w:st)
                                Just d  -> descend (l, (body d) ++ st)
primEval t@(l, Fun f:st) = primApply t -- f (l, st)
primEval (l, Comm _:st)  = (l, st)
primEval (l, e:st)       = (l, e:st)
 
primError :: State -> State
primError (l, msg:st) = barf st (show msg)

descend :: State -> State
descend (l, []) = (l, [])
descend (l, e:st) = primEval (l', e:st')
    where
        (l', st') = descend (l, st)

numericBinaryPrim :: (Int -> Int -> Int) -> State -> State
numericBinaryPrim f (l, (Num x  : Num y  : st')) = (l, Num  (f x y) : st')
numericBinaryPrim f (l, st) = (l, Fun (numericBinaryPrim f):st)

-- wrapper for functions that don't manipulate the library

makeStateful :: (Prog -> Prog) -> State -> State
makeStateful f (l, st) = (l, f st)

makeDefn :: (String, Prog -> Prog) -> Defn
makeDefn (id, f) = Defn id [Fun $ makeStateful f]

prims = (map makeDefn progFunctions) ++ [
    Defn "dip" [Fun primDip],
    Defn "dig" [Fun primDig],
    Defn "apply" [Fun primApply],

    Defn "+" [Fun $ numericBinaryPrim (+)],
    Defn "-" [Fun $ numericBinaryPrim (-)],
    Defn "*" [Fun $ numericBinaryPrim (*)],
    Defn "/" [Fun $ numericBinaryPrim div],
    Defn "%" [Fun $ numericBinaryPrim mod],

    Defn "error!" [ Fun primError ],

    Defn "def" [Fun primDef] ]




---------------------------------------------------------------------
-- Argument processing and main
---------------------------------------------------------------------

barf :: Prog -> String -> a
barf st msg = error $ "** Error: " ++ msg ++ "\nAt\n   " ++ showAst st ++ "\n\n"

showAst :: Prog -> String
showAst st = ( take 100 $ show $ Quot st ) ++ ( if length st > 100 then "..." else "" )


main :: IO ()
main = do
    args <- getArgs
    sources <- mapM readFile (args ++ ["headstone.fn"])
    let prog = parse $ concat sources --args
    let lib = prims
    putStrLn $ show $ snd $ descend (lib, trace (showAst prog) prog)


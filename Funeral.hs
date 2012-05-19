module Main where

import System( getArgs )
import Char
import IO (readFile)
import List (nubBy, partition, intersperse, find)

import Debug.Trace

import Funeral.ParseLib

data Expr = Word Id
          | Quot [Expr]
          | Bool Bool
          -- | Pair Expr Expr
          | Chr Char
          | Num Int
          | Fun (State -> State)
          | Def Id [Expr]
          | Comm String
data Definition = Defn Id Prog

instance Eq Expr where
    Word w == Word x    =    w == x
    Num  n == Num  m    =    n == m
    Bool b == Bool c    =    b == c
    Chr  c == Chr  d    =    c == d
    Quot es == Quot xs  =    es == xs
    -- Pair e f == Pair x y =   e == x && x == y
    Fun _ == Fun _      =    error "Cannot compare functions"
    Def _ _ == Def _ _  =    error "Cannot compare definitions"
    _      == _         =    False

instance Ord Expr where
    Word w <= Word x    =    w <= x
    Num  n <= Num  m    =    n <= m
    Bool b <= Bool c    =    b <= c
    Chr  c <= Chr  d    =    c <= d
    Quot es <= Quot xs  =    es <= xs
    -- Pair e f == Pair x y =   e == x && x == y
    Fun _ <= Fun _      =    error "Cannot compare functions"
    Def _ _ <= Def _ _  =    error "Cannot compare definitions"
    _      <= _         =    error "Cannot compare values of differing types"
    

instance Show Expr where
    show (Word id) = id
    show (Num n) = show n
    show (Bool b) = show b
    -- show (Pair a b) = "(" ++ show a ++ " " ++ show b ++ ")"
    show (Chr c) = '.':c:[]
    show (Quot es) = case all typeIsChar es of
            True  -> "\"" ++ ( map (\(Chr c) -> c) es ) ++ "\""
            False -> "[" ++ (concat $ intersperse " " $ map show es) ++ "]"
        where
            typeIsChar (Chr _) = True
            typeIsChar _       = False
    show (Fun _) = "<function>"
    show (Def id es) = "def " ++ id ++ " " ++ ( show $ Quot es )
    show (Comm s) = "" -- "-- " ++ s ++ "\n"


type Id = String
type Prog = [ Expr ]
type Library = [ Definition ]
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
--         <|> token ( char '\'' |> maybeSome ( satisfy (/= '\'') ) <| char '\'' )

parseNumber :: Parser Expr
parseNumber = pure Num <*> n
    where
        n = token $ pure (read) <*> atLeastOne digit

parseComment :: Parser Expr
parseComment = token $ pure Comm <*> keyword "--" |> ( maybeSome $ anyCharExcept "\n\r" ) <| ( maybeSome $ satisfy (`elem` "\n\r") )

-- parsePair :: Parser Expr
-- parsePair = pure Pair <*> keyword "(" |> parseExpr <*> parseExpr <| keyword ")"

parseId :: Parser String
parseId = token $ pure (:) <*> anyCharExcept reservedPrefixes <*> ( maybeSome $ anyCharExcept reservedChars )
    where
        reservedPrefixes = "'.0123456789" ++ reservedChars
        reservedChars = " \t\n\r\0()[]\"\'"

parseBool :: Parser Expr
parseBool = pure Bool <*> ( pure read <*> ( keyword "True" <|> keyword "False" ) )

parseWord :: Parser Expr
parseWord = pure Word <*> parseId

parseQuot :: Parser Expr
parseQuot = token $ pure Quot <*> quo
    where
        quo = keyword "[" |> maybeSome parseExpr <| keyword "]"
          <|> char '\'' |> exactlyOne parseExpr

parseExpr :: Parser Expr
parseExpr = parseComment <|> parseBool <|> parseChar <|> parseNumber <|> parseQuot {- <|> parsePair -} <|> parseString <|> parseWord

parse :: String -> [Expr]
parse s = case junk of
    ""        -> exps
    otherwise -> error ("Parse error in '" ++ take 30 junk ++ "...'\n")
    where
        (junk, exps) = head $ maybeSome parseExpr s

---------------------------------------------------------------------
-- Primitive functions
---------------------------------------------------------------------

-- Stack manipulation

fnDrop :: Prog -> Prog
fnDrop (e:st) = st
fnDrop [] = barf [] "drop called on an empty stack."

fnDup :: Prog -> Prog
fnDup (e:st) = e:e:st
fnDup [] = barf [] "dup called on an empty stack."

fnBury :: Prog -> Prog
fnBury (Num n:e:st) = es ++ (e:st')
    where
        (es, st') = splitAt n st

fnExhume :: Prog -> Prog
fnExhume (Num n:st) = (e:st')
    where
        (xs, e:ys) = splitAt n st
        st' = xs ++ ys

stackManipulationFunctions = [
    ( "drop", promoteProgFn $ fnDrop ),
    ( "dup",  promoteProgFn $ fnDup ),
    ( "bury", promoteProgFn $ fnBury ),
    ( "exhume", promoteProgFn $ fnExhume ) ]


-- Logical

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

truthValueOf :: Expr -> Bool
truthValueOf (Bool False) = False
truthValueOf e = True

logicalFunctions = [
    ( "not",  promoteProgFn $ fnNot ),
    ( "or",   promoteProgFn $ fnOr ),
    ( "and",  promoteProgFn $ fnAnd ) ]

-- Comparison

comparisonFunctions = [
    ( "=",    promoteProgFn $ \(f:e:st) -> Bool (e == f):st ),
    ( "!=",   promoteProgFn $ \(f:e:st) -> Bool (e /= f):st ),
    ( ">",    promoteProgFn $ \(f:e:st) -> Bool (e > f):st ),
    ( "<",    promoteProgFn $ \(f:e:st) -> Bool (e < f):st ),
    ( ">=",   promoteProgFn $ \(f:e:st) -> Bool (e >= f):st ),
    ( "<=",   promoteProgFn $ \(f:e:st) -> Bool (e <= f):st ) ]

-- Arithmetic

arithmeticFunctions = [
    ("+", promoteProgFn $ numericBinaryPrim (+)),
    ("-", promoteProgFn $ numericBinaryPrim (-)),
    ("*", promoteProgFn $ numericBinaryPrim (*)),
    ("/", promoteProgFn $ numericBinaryPrim div),
    ("%", promoteProgFn $ numericBinaryPrim mod) ]

-- List manipulation

fnNull :: Prog -> Prog
fnNull (Quot []:st) = Bool True:st
fnNull (Quot q:st)  = Bool False:st
fnNull (e:st) = barf st ("Cannot determine if non-quote value '" ++ show e ++ "' is null.")

fnSplitAt :: Prog -> Prog
fnSplitAt (Num n:Quot es:st) = Quot as:Quot bs:st
    where
        (as, bs) = splitAt n es

fnCons :: Prog -> Prog
fnCons (e:Quot es:st) = Quot (e:es):st
fnCons st = barf st $ "Attempted to cons onto something other than a quotation."

fnUncons :: Prog -> Prog
fnUncons (Quot (e:es):st) = e:Quot es:st
fnUncons st@(e:_) = barf st $ "uncons called on a non-quotation value."

fnQuote :: Prog -> Prog
fnQuote (e:st) = Quot [e]:st
fnQuote [] = barf [] $ "Called quote on an empty stack"

fnAppend :: Prog -> Prog
fnAppend (Quot b:Quot a:st) = Quot (a ++ b):st
fnAppend (b:Quot a:st)      = Quot (a ++ [b]):st
fnAppend (b:a:st)           = Quot [a, b]:st -- not sure whether this is a useful convenience or a debugging nightmare!

listManipulationFunctions = [
    ( "quote",   promoteProgFn $ fnQuote ),
    ( "cons",    promoteProgFn $ fnCons ),
    ( "uncons",  promoteProgFn $ fnUncons ),
    ( "splitAt", promoteProgFn $ fnSplitAt ),
    ( "null",    promoteProgFn $ fnNull ),
    ( "append",  promoteProgFn $ fnAppend ) ]


-- Misc functions


fnError :: Prog -> Prog
fnError (msg:st) = barf st (show msg)

fnShow :: Prog -> Prog
fnShow (e:st) = (strToQuote $ show e):st
fnShow [] = []

fnType :: Prog -> Prog
fnType (Quot [Quot _]:st)   = strToQuote "quotation":st
fnType (Quot [Word _]:st)   = strToQuote "word":st
fnType (Quot [Bool _]:st)   = strToQuote "boolean":st
fnType (Quot [Chr _]:st)    = strToQuote "character":st
fnType (Quot [Fun _]:st)    = strToQuote "function":st -- Without quotation Fun consumes its args.
fnType (Quot [Num _]:st)    = strToQuote "number":st
fnType (Quot [Def _ _]:st)  = strToQuote "definition":st
fnType st = barf st "type must be called on a quoted value."

fnIsString :: Prog -> Prog
fnIsString (Quot es:st)     = if all isChar es then Bool True:st else Bool False:st
	where
		isChar (Chr _) = True
		isChar _ = False
fnIsString (e:st) = Bool False:st

miscFunctions = [
    ( "show",     promoteProgFn $ fnShow ),
	( "isString", promoteProgFn $ fnIsString ),
    ( "type",     promoteProgFn $ fnType ),
    ( "croak",    promoteProgFn $ fnError ) ]

-- State -> State functions

fnDef :: State -> State
fnDef (l, Word id:Quot es:st)        = (Defn id es:l, st)
fnDef (l, Quot [Word id]:Quot es:st) = (Defn id es:l, st) -- Need to pre-process and sub-in 
fnDef (l, Word id:e:st)  = barf st "Definitions must be quoted"
fnDef (l, Fun _:st) = barf st "Attempted to redefine a word."
fnDef (l, e:st) = barf st $ "Can't define " ++ show e

fnApply :: State -> State
fnApply (l, Fun f:st)     = descend (l', st')
    where
        (l', st') = f (l, st)
fnApply (l, Quot es:st)   = descend (l, es ++ st)
fnApply (l, e:st)         = barf st $ "Don't know how to apply " ++ show e

fnDig :: State -> State
fnDig (l, Num n:Quot q:st) = (l', es ++ st')
    where
        es = take n st
        (l', st') = fnApply (l, Quot q:(drop n st))

fnDefined :: State -> State
fnDefined (l, Quot [Word w]:st) = case getContext w l of
    [] -> (l, Bool False:st)
    _  -> (l, Bool True:st)

fnEval :: State -> State
fnEval t@(l, [])           = t
fnEval (l, Word w:st)  = case getContext w l of
                          []                -> (l, Word w:st)
                          ctx@(Defn _ es:_) -> descend (ctx, es ++ st)
fnEval t@(l, Fun f:st) = fnApply t
fnEval (l, Comm _:st)  = (l, st)
fnEval (l, e:st)       = (l, e:st)
 
-- utility functions

getContext :: Id -> Library -> Library
getContext w l = dropWhile notMyDef l
    where
        notMyDef (Defn id _) = id /= w

descend :: State -> State
descend (l, []) = (l, [])
descend (l, e:st) = fnEval (l', e:st')
    where
        (l', st') = descend (l, st)

numericBinaryPrim :: (Int -> Int -> Int) -> Prog -> Prog
numericBinaryPrim f (Num y  : Num x  : st) = (Num  (f x y) : st)
-- numericBinaryPrim f st = Fun (promoteProgFn $ numericBinaryPrim f):st

primDef :: (String, State -> State) -> Definition
primDef (id, f) = Defn id [Fun f]

promoteProgFn :: (Prog -> Prog) -> State -> State
promoteProgFn f (l, st) = (l, f st)

---------------------------------------------------------------------
-- Primitive definitions
---------------------------------------------------------------------
stateFunctions = [
    
    --("dip", fnDip),
    ("dig", fnDig),
    ("apply", fnApply),

    ("defined", fnDefined ),
    ("eval", fnEval ),
    ("def", fnDef ) ]

prims = map primDef stateFunctions




---------------------------------------------------------------------
-- Argument processing and main
---------------------------------------------------------------------

barf :: Prog -> String -> a
barf st msg = error $ "** Error: " ++ msg ++ "\nAt\n   " ++ showAst st ++ "\n\n"

formatExpr :: Expr -> String
formatExpr (Def _ _) = ""
formatExpr e = show e

showAst :: Prog -> String
showAst st = concat $ intersperse "\n" $ filter (/= "") $ map formatExpr st


main :: IO ()
main = do
    args <- getArgs
    sources <- mapM readFile (args ++ ["headstone.fn"])
    let prog = ( parse $ concat sources )
    putStrLn $ showAst $ snd $ descend (prims, prog)


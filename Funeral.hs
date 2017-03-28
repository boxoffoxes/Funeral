module Main where

import Prelude hiding (pure, (<*>))
import System.IO (readFile)
import System.Environment ( getArgs )
import Data.Char
import Data.List (nubBy, partition, intersperse)

import Debug.Trace

import Funeral.ParseLib

data Expr = Word Id
          | Quot [Expr]
          | Bool Bool
          -- | Pair Expr Expr
          | Chr Char
          | Num Int
          | Fun Id (State -> State)
          -- | Def Id [Expr]
          | Comm String
data Definition = Defn Id Prog deriving Show

instance Eq Expr where
    Word w == Word x    =    w == x
    Num  n == Num  m    =    n == m
    Bool b == Bool c    =    b == c
    Chr  c == Chr  d    =    c == d
    Quot es == Quot xs  =    es == xs
    -- Pair e f == Pair x y =   e == x && f == y
    Fun _ _ == Fun _ _  =    error "Cannot compare functions"
    -- Def _ _ == Def _ _  =    error "Cannot compare definitions"
    _      == _         =    False

instance Ord Expr where
    Word w <= Word x    =    w <= x
    Num  n <= Num  m    =    n <= m
    Bool b <= Bool c    =    b <= c
    Chr  c <= Chr  d    =    c <= d
    Quot es <= Quot xs  =    es <= xs
    -- Pair e f <= Pair x y =   e <= x && f <= y
    Fun _ _ <= Fun _ _  =    error "Cannot compare functions"
    -- Def _ _ <= Def _ _  =    error "Cannot compare definitions"
    _      <= _         =    error "Cannot compare values of differing types"
    

instance Show Expr where
    show (Word id) = id
    show (Num n) = show n
    show (Bool b) = show b
    -- show (Pair a b) = "(" ++ show a ++ " " ++ show b ++ ")"
    show (Chr c) = '.':c:[]
    show (Quot es) = case all isChar es of
            True  -> show ( map (\(Chr c) -> c) es )
            False -> "[" ++ (concat $ intersperse " " $ map show es) ++ "]"
    show (Fun id _) = ":" ++ id
    -- show (Def id es) = "def " ++ id ++ " " ++ ( show $ Quot es )
    show (Comm s) = "" -- "-- " ++ s ++ "\n"


type Id = String
type Prog = [ Expr ]
type Library = [ Definition ]
type Input = [ String ]
type Output = [ String ]
type State = (Library, Prog, Input, Output)

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

stripComments :: [Expr] -> [Expr]
stripComments (Comm _:es) = stripComments es
stripComments (Quot qs:es) = Quot (stripComments qs):stripComments es
stripComments (e:es) = e : stripComments es
stripComments [] = []

parse :: String -> [Expr]
parse s = case junk of
    ""        -> stripComments exps
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
fnSplitAt st = barf st "Couldn't split a non-quote value"

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

fnTrace :: Prog -> Prog
fnTrace (e:st) = trace ("+++ Trace: " ++ (show e) ) (e:st)

fnTraceAll :: Prog -> Prog
fnTraceAll st = trace (" +++ TraceAll: " ++ (show $ Quot st) ) st

fnShow :: Prog -> Prog
fnShow (e:st) = (strToQuote $ show e):st
fnShow [] = []

fnType :: Prog -> Prog
fnType (Quot [Quot _]:st)   = strToQuote "quotation":st
fnType (Quot [Word _]:st)   = strToQuote "word":st
fnType (Quot [Bool _]:st)   = strToQuote "boolean":st
fnType (Quot [Chr _]:st)    = strToQuote "character":st
fnType (Quot [Fun _ _]:st)  = strToQuote "function":st -- Without quotation Fun consumes its args.
fnType (Quot [Num _]:st)    = strToQuote "number":st
-- fnType (Quot [Def _ _]:st)  = strToQuote "definition":st
fnType st = barf st "type must be called on a quoted value."

fnIsString :: Prog -> Prog
fnIsString (Quot es:st)     = if all isChar es then Bool True:st else Bool False:st
fnIsString (e:st) = Bool False:st

fnWordToString :: Prog -> Prog
fnWordToString (Word w:st) = strToQuote w:st
fnWordToString (Quot [Word w]:st) = strToQuote w:st
fnWordToString st = barf st "wordToString requires either a word or a quoted word argument."

fnStringToWord :: Prog -> Prog
fnStringToWord (Quot es:st)
	| all isChar es = Word (map (\(Chr c) -> c) es):st
fnStringToWord st = barf st "stringToWord only accepts string argument."

miscFunctions = [
    ( "show",     promoteProgFn $ fnShow ),
    ( "trace",    promoteProgFn $ fnTrace ),
    ( "traceAll", promoteProgFn $ fnTraceAll ),
	( "isString", promoteProgFn $ fnIsString ),
    ( "type",     promoteProgFn $ fnType ),
	( "stringToWord", promoteProgFn $ fnStringToWord ),
	( "wordToString", promoteProgFn $ fnWordToString ),
    ( "croak",    promoteProgFn $ fnError ) ]

-- State -> State functions

fnForget :: State -> State
fnForget (l, Quot [Word id]:st, i, o) = (l', st, i, o)
    where
        l' = tail $ getContext id l
fnForget t = barfPlus t "forget takes a single quoted word as an argument."

fnDef :: State -> State
fnDef (l, Word id:Quot es:st, i, o)        = (Defn id (expand l es):l, st, i, o)
fnDef (l, Quot [Word id]:Quot es:st, i, o) = (Defn id (expand l es):l, st, i, o) -- Need to pre-process and sub-in 
fnDef t@(l, Word id:e:st, i, o)  = barfPlus t "Definitions must be quoted"
--fnDef (l, Fun id _:st) = barf st $ "Attempted to redefine the word " ++ id ++ ". If you meant to do this call \n\tdef '" ++ id ++ " [...\ninstead"
fnDef t@(l, e:st, i, o) = barfPlus t $ "Can't define " ++ show e

fnDig :: State -> State
fnDig (l, Num n:Quot q:st, i, o) = (l', es ++ st', i, o)
    where
        es = take n st
        (l', st', i', o') = fnApply (l, Quot q:(drop n st), i, o)

fnDefined :: State -> State
fnDefined (l, Quot [Word w]:st, i, o) = case getContext w l of
                                    [] -> (l, Bool False:st, i, o)
                                    _  -> (l, Bool True:st, i, o)
fnDefined t@(l, st, i, o) = barfPlus t "defined called on a value other than a quoted word"

-- Apply a function or quotation to transform the AST
fnApply :: State -> State
fnApply t@(l, Fun _ f:st, i, o)     = descend $ f (l, st, i, o)
fnApply (l, Quot es:st, i, o)       = descend (l, es ++ st, i, o)
fnApply t@(l, e:st, i, o)             = barfPlus t $ "Don't know how to apply " ++ show e

-- Work out what to do with each different type of expression encountered in the AST:
-- possibilities are: leave unchanged, delete, apply, or descend
fnEval :: State -> State
fnEval t@(l, [], i, o)       = t
fnEval (l, Word w:st, i, o)  = case getContext w l of
                          []                -> (l, Word w:st, i, o)
                          (Defn _ es:_)     -> descend (l, es ++ st, i, o)
fnEval t@(l, Fun _ f:st, i, o) = fnApply t
fnEval (l, Comm _:st, i, o)  = (l, st, i, o)
fnEval (l, e:st, i, o)       = (l, e:st, i, o)

-- Descend the program's AST looking for expressions that can be evaluated.
descend :: State -> State
descend t@(l, [], i, o) = t
descend (l, e:st, i, o) = fnEval (l', e:st', i', o')
    where
        (l', st', i', o') = descend (l, st, i, o)

fnPrint :: State -> State
fnPrint (l, e:st, i, o) = (l, st, i, format e:o)
fnPrint t@(_, [], i, o)   = barfPlus t "Attempt to call 'print' on empty stack"


-- utility functions
isChar (Chr _) = True
isChar _ = False

-- return the subset of a library visible to a particular word
getContext :: Id -> Library -> Library
getContext w l = dropWhile notMyDef l
    where
        notMyDef (Defn id _) = id /= w

-- Recursively replace Quotations and defined Words with their definitions. For 
-- use in defining a new word
expand :: Library -> Prog -> Prog
expand l (Word w:st) = case getContext w l of
                        [] -> Word w : expand l st  -- word not defined. Push literal
                        (Defn _ es : l') -> expand l' es ++ expand l st
expand l (Quot q:st) = Quot (expand l q) : expand l st
expand l (e:st)      = e:(expand l st)
expand l []          = []


-- Promote a Haskell binary prim to a side-effect-free word
numericBinaryPrim :: (Int -> Int -> Int) -> Prog -> Prog
numericBinaryPrim f (Num y  : Num x  : st) = (Num  (f x y) : st)
-- numericBinaryPrim f st = Fun _ (promoteProgFn $ numericBinaryPrim f):st

primDef :: (String, State -> State) -> Definition
primDef (id, f) = Defn id [Fun id f]

promoteProgFn :: (Prog -> Prog) -> State -> State
promoteProgFn f (l, st, i, o) = (l, f st, i, o)

format :: Expr -> String
format (Quot es) = case all isChr es of
        True -> map (\(Chr c) -> c) es
        False -> error $ "Don't know how to format a non-string quotation: " ++ show (Quot es)
    where
        isChr (Chr _) = True
        isChr _ = False
format (Num n) = show n
format e = ""

---------------------------------------------------------------------
-- Primitive definitions
---------------------------------------------------------------------
stateFunctions = stackManipulationFunctions 
              ++ logicalFunctions 
              ++ comparisonFunctions 
              ++ arithmeticFunctions
              ++ listManipulationFunctions
              ++ miscFunctions
              ++ [ ("dig", fnDig),
                   ("forget", fnForget),
                   ("apply", fnApply),
                   ("defined", fnDefined ),
                   ("eval", fnEval ),
                   ( "print", fnPrint ),
                   ("def", fnDef ) ]

prims = map primDef stateFunctions




---------------------------------------------------------------------
-- Argument processing and main
---------------------------------------------------------------------

barf :: Prog -> String -> a
barf st msg = error $ "** Error: " ++ msg ++ "\nAt\n   " ++ showAst st ++ "\n\n"

barfPlus :: State -> String -> a
barfPlus t msg = error $ "** Error: " ++ msg ++ "\nAt\n " ++ showState t ++ "\n\n"

showAst :: Prog -> String
showAst st = concat $ intersperse "\n" $ map show st

showState :: State -> String
showState t@(l, st, i, o) = concat [ "\n ++ Library: " ++ (concat $ intersperse "\n\t" $ map show l) , "\n ++ Unprocessed AST: " ++ show st , "\n ++ Unprocessed input: " ++ show i , "\n ++ Generated output: " ++ show o ]

showOutput :: State -> String
showOutput (_, _, _, os) = concat $ intersperse "\n" os

main :: IO ()
main = do
	-- hSetEncoding stderr utf8
    args <- getArgs
--    sources <- mapM readFile (args ++ ["lib/headstone.fn"])
    sources <- mapM readFile args
    let prog = ( parse $ concat sources )
    let result = descend (prims, prog, [], [])
    let output = case result of 
            ( _, [], _, _ ) -> showOutput result
            _               -> showState result
    putStrLn output


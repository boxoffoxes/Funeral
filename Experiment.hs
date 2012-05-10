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
          -- | Str String
          | Chr Char
          | Num Int
          | Fun (State -> State)

instance Eq Expr where
    Word w == Word x    =    w == x
    Num  n == Num  m    =    n == m
    Bool b == Bool c    =    b == c
    Chr  c == Chr  d    =    c == d
    Quot es == Quot xs  =    es == xs
    Fun  _ == Fun _     =    error "Cannot compare functions"
    _      == _         =    False
    

instance Show Expr where
    show (Word id) = id
    show (Num n) = show n
    show (Bool b) = '?' : show b
    show (Chr c) = '.':c:[]
    show (Quot es) = "[" ++ (concat $ intersperse " " $ map show es) ++ "]"
    show (Fun _) = "<function>"


data Defn = Defn {
        name :: Id,
        arity :: Int,
        body :: Expr
    } deriving Show


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

strToQuote :: String -> Expr
strToQuote s = Quot (map Chr s)

token :: Parser a -> Parser a
token p = p <| ignoredChars
    where
        ignoredChars = maybeSome $ satisfy (`elem` " \t;,\n\r")

keyword :: String -> Parser String
keyword = token . string

parseChar :: Parser Expr
parseChar = pure Chr <*> char '.' |> anyChar

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

parseId :: Parser String
parseId = token $ atLeastOne ( satisfy (not . (elem' " \t\n\r[]\"'`") ) )
    where
        elem' a b = elem b a

parseBool :: Parser Expr
parseBool = pure Bool <*> ( pure read <*> ( keyword "True" <|> keyword "False" ) )

parseWord :: Parser Expr
parseWord = pure Word <*> parseId

parseQuot :: Parser Expr
parseQuot = pure Quot <*> keyword "[" |> maybeSome parseExpr <| keyword "]"

parseExpr :: Parser Expr
parseExpr = parseBool <|> parseChar <|> parseNumber <|> parseQuot <|> parseString <|> parseWord

parse :: String -> [Expr]
parse s = case junk of
    ""        -> exps
    otherwise -> error ("Parse error in '" ++ take 30 junk ++ "...'\n")
    where
        (junk, exps) = head $ maybeSome parseExpr s

---------------------------------------------------------------------
-- Primitive functions
---------------------------------------------------------------------

-- wrapper for functions that don't manipulate the library
makeStateful :: (Stack -> Stack) -> State -> State
makeStateful f (l, st) = (l, f st)

primDef :: State -> State
primDef (l, Word id:e:st') = (l', st')
    where
        l' = (Defn id 0 e) : l -- TODO: arity

primType :: State -> State
primType (l, st@(Quot _:_)) = (l, strToQuote "quotation":st)
primType (l, st@(Word _:_)) = (l, strToQuote "word":st)
primType (l, st@(Bool _:_)) = (l, strToQuote "boolean":st)
primType (l, st@(Chr _:_))  = (l, strToQuote "character":st)
primType (l, st@(Fun _:_))  = (l, strToQuote "function":st)
primType (l, st@(Num _:_))  = (l, strToQuote "number":st)

primNot :: State -> State
primNot (l, Bool b:st) = (l, Bool (not b):st)

primEq :: State -> State
primEq ( l, Word a : Word b : st ) = (l, Bool (a == b) : st)
primEq ( l, Bool a : Bool b : st ) = (l, Bool (a == b) : st)
primEq ( l, Num  a : Num  b : st ) = (l, Bool (a == b) : st)
primEq ( l, Chr  a : Chr  b : st ) = (l, Bool (a == b) : st)
primEq ( l, a      : b      : st ) = (l, Bool False : st)

primDip :: State -> State
primDip (l, Quot q : e : es ) = (l', e:es')
    where
        (l', es') = primEval (l, es)

primDrop :: State -> State
primDrop (l, _:st) = (l, st)

primSwap :: State -> State
primSwap (l, a:b:st) = (l, b:a:st)

primRot :: State -> State
primRot (l, a:b:c:st) = (l, c:a:b:st)

primDup :: State -> State
primDup (l, a:st) = (l, a:a:st)

{-primMap :: State -> State
primMap (l, Fun f:Quot es:st) = (l, Quot es':st)
    where
        es' = map f es

primFold :: State -> State
primFold (l, Fun f:Quot es:e:st) = (l, e':st)
    where
        e' = foldr f es
-}

primApply :: State -> State
primApply (l, Fun f:st) = primEval $ f (l, st) -- what if we only apply fully-saturated functions?
primApply (l, Quot es:st) = primEval (l, es ++ st)
primApply (l, st) = primEval (l, st)

primEval :: State -> State -- do this depth-first!
primEval (l, []) = (l, [])
primEval (l, Word w:st) = case find (\(Defn id a f) -> id == w) l of
    Nothing -> (l, Word w:st)
    Just d  -> primEval (l, (body d):st)
primEval (l, Fun f:es) = f (l, es)
primEval (l, e:st) = (l, e:st)

descend :: State -> State
descend (l, []) = (l, [])
descend (l, e:st) = primEval (l', e:st')
    where
        (l', st') = descend (l, st)

{-
primEval (l, Word w:st) = case find (\(Defn id a f) -> id == w) l of
    Nothing -> error $ "undefined word " ++ w
    Just d  -> primEval (l', (body d):st')
        where
            (l', st') = primEval (l, st)
primEval (l, st@(Fun f:Fun g:_)) = primEval $ primCompose (l, st)
-- primEval (l, st@(Fun f:_)) = primEval $ primApply (l, st) -- wrong! needs to work from the inside out.
primEval (l, e:es) = (l', e:es')
    where
        (l', es') = primEval (l, es)
primEval (l, []) = (l, [])
-}

primCompose :: State -> State
primCompose (l, Fun f:Fun g:st) = (l, Fun (f . g):st)

primQuote :: State -> State
primQuote (l, e:st) = (l, Quot [e]:st)

primCons :: State -> State
primCons (l, e:Quot es:st) = (l, Quot (e:es):st)

primHeadTail :: State -> State
primHeadTail (l, (Quot (e:es):st)) = (l, e:Quot es:st)

primTake :: State -> State
primTake (l, Num n:(Quot es):st) = (l, Quot (take n es):st)

primStrip :: State -> State
primStrip (l, Num n:(Quot es):st) = (l, Quot (drop n es):st)

primOr :: State -> State
primOr (l, x:y:st) = case truthValueOf x of
    True  -> (l, x:st)
    False -> (l, y:st)
        
primAnd :: State -> State
primAnd (l, x:y:st) = case truthValueOf x of
    False -> (l, x:st)
    True  -> (l, y:st)

truthValueOf :: Expr -> Bool
truthValueOf (Bool False) = False
truthValueOf e = True

numericBinaryPrim :: (Int -> Int -> Int) -> State -> State
numericBinaryPrim f (l, (Num x  : Num y  : st')) = (l, Num  (f x y) : st')
numericBinaryPrim f (l, st) = (l, Fun (numericBinaryPrim f):st)

prims = [
    Defn "+" 2 (Fun $ numericBinaryPrim (+)),
    Defn "-" 2 (Fun $ numericBinaryPrim (-)),
    Defn "*" 2 (Fun $ numericBinaryPrim (*)),
    Defn "/" 2 (Fun $ numericBinaryPrim div),
    Defn "%" 2 (Fun $ numericBinaryPrim mod),

    Defn "not" 1 (Fun primNot),
    Defn "or"  2 (Fun primOr),
    Defn "and" 2 (Fun primAnd),
    Defn "="   2 (Fun primEq),
    
    Defn "swap" 2 (Fun primSwap),
    Defn "dip"  2 (Fun primDip),
    Defn "drop" 1 (Fun primDrop),
    Defn "rot"  2 (Fun primRot),
    Defn "dup"  1 (Fun primDup),

	Defn "apply"   1 (Fun primApply),
    Defn "compose" 2 (Fun primCompose),
--    Defn "eval" ? (Fun primEval),

    Defn "cons"   2 (Fun primCons),
    Defn "uncons" 1 (Fun primHeadTail),

    Defn "type" 1 (Fun primType),

    Defn "def" 2 (Fun primDef) ]




---------------------------------------------------------------------
-- AST transformation and evaluation
---------------------------------------------------------------------

{- transform :: State -> State
transform (l, Word w:st) = case find (\(Defn id f) -> id == w) l of
    Just (Defn _ e) -> transform (l, e:st)
    Nothing -> (l, Word w : st) -- eval st first?
        -- where
            -- (l', st') = eval (l, st)
transform (l, Fun f:Fun g:st) = transform $ primCompose
transform (l, Quot [e]:st) = transform (l, e:st)
transform (l, Quot es:st) = transform (l, es)
transform (l, e:es) = (l', e:es')
    where
        (l', es') = transform (l, es)
transform (l, []) = (l, [])

eval :: State -> State
eval (l, Word w:st) = case find (\(Defn id f) -> id == w) l of
    Just d  -> eval (l, body d : st)
    Nothing -> (l', Word w : st')
        where
            (l', st') = eval (l, st)
eval (l, Fun f : Fun g : st) = (l, Fun (f . g) : st)
eval (l, [Fun f]) = (l, [Fun f])
eval (l, Fun f : st) = eval $ f $ eval (l, st)
-- eval (l, Quot [Fun f]:st) = eval (l, Fun f:st)
-- eval (l, Quot es:st) = (l', Quot es':st')
    -- where
        -- (_, es') = eval (l, es)
        -- (l', st') = eval (l, st)
-- eval (l, Quot es:st) = eval (l, Quot f:st)
-- eval (l, Quot es:st) = eval (l, es ++ st)
eval (l, e:es) = (l', e:es')
    where
        (l', es') = eval (l, es)
eval (l, []) = (l, [])
-}

main :: IO ()
main = do
    args <- getArgs
    sources <- mapM readFile (args ++ ["headstone.fun"])
    let stack = parse $ concat sources --args
    let lib = prims
    putStrLn $ show $ descend (lib, trace (show stack) stack)


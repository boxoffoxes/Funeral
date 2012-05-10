module Main where

import System( getArgs )
import Char
import IO (readFile)
import List (nubBy, partition, intersperse, find)

import Author.ParseLib

data Expr = Word Id
          | Quot [Expr]
          | Bool Bool
          -- | Str String
          | Chr Char
          | Num Int
          | Fun (State -> State)

instance Show Expr where
    show (Word id) = id
    show (Num n) = show n
    show (Bool b) = '?' : show b
    show (Chr c) = '.':c:[]
    show (Quot es) = "[" ++ (concat $ intersperse " " $ map show es) ++ "]"
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

primDef :: State -> State
primDef (l, Word id:e:st') = (l', st')
    where
        l' = (Defn id e) : l

primTail :: State -> State
primTail (l, (Quot (e:es) : st')) = (l, Quot es : st')

--primConcat :: State -> State
--primConcat (l, Quot es:st') = (l, Str (concat $ map show es) : st')

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

primEval :: State -> State
primEval (l, Word w:st) = case find (\(Defn id f) -> id == w) l of
    Nothing -> error $ "undefined word " ++ w
    Just d  -> primEval (l, (body d):st)
primEval (l, st@(Fun f:Fun g:_)) = primEval $ primCompose (l, st)
-- primEval (l, st@(Fun f:_)) = primEval $ primApply (l, st) -- wrong! needs to work from the inside out.
primEval (l, e:es) = (l', e:es')
    where
        (l', es') = primEval (l, es)
primEval (l, []) = (l, [])

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
    Defn "+" (Fun $ numericBinaryPrim (+)),
    Defn "-" (Fun $ numericBinaryPrim (-)),
    Defn "*" (Fun $ numericBinaryPrim (*)),
    Defn "/" (Fun $ numericBinaryPrim div),
    Defn "%" (Fun $ numericBinaryPrim mod),

    Defn "or"  (Fun primOr),
    Defn "and" (Fun primAnd),
    Defn "not" (Fun primNot),
    Defn "="  (Fun primEq),
    
    Defn "swap" (Fun primSwap),
    Defn "dip" (Fun primDip),
    Defn "drop" (Fun primDrop),
    Defn "rot" (Fun primRot),
    Defn "dup" (Fun primDup),

	Defn "apply" (Fun primApply),
    Defn "compose" (Fun primCompose),
    Defn "eval" (Fun primEval),

    Defn "cons" (Fun primCons),
    Defn "uncons" (Fun primHeadTail),
    Defn "strip" (Fun primStrip),

    Defn "type" (Fun primType),

    Defn "def" (Fun primDef) ]




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
    sources <- mapM readFile ("headstone.fun":args)
    let stack = parse $ concat sources --args
    let lib = prims
    putStrLn $ show $ primEval (lib, stack)


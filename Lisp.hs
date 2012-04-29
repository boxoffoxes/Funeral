module Lisp where

import HtmlGen.ParseLib

import IO
import System( getArgs )
import Char (isDigit, isAlpha, isAlphaNum, isSpace)

data Expr	= Atom String
			| List [Expr]
			| DottedList [Expr] Expr
			| Num Integer
			| Str String
			| Boo Bool
--	deriving (Show, Eq)

instance Show Expr where show = showVal


symbol :: Parser Char
symbol = satisfy (`elem` "!#$%&|*+-/:<=>?@^_~") 

digit :: Parser Char
digit = satisfy isDigit

letter :: Parser Char
letter = satisfy isAlpha


token :: Parser a -> Parser a
token p = p <| maybeSome space

keyword :: String -> Parser String
keyword = token . string


parseString :: Parser Expr
parseString = pure Str <*> parseString'
	where
		parseString' = char '"' |> maybeSome ( satisfy (/= '"') ) <| char '"' 

parseAtom :: Parser Expr
parseAtom = pure Atom <*> label
	where
		label = pure (:) <*> first <*> rest
		first = letter <|> symbol
		rest = maybeSome ( letter <|> symbol <|> digit )

parseNumber :: Parser Expr
parseNumber = pure Num <*> n
	where
		n = pure (read) <*> atLeastOne digit

parseExpr :: Parser Expr
parseExpr = token $ parseAtom 
				<|> parseString 
				<|> parseNumber 
				<|> parseQuoted 
				<|> keyword "(" |> parseLists <| keyword ")"
				<|> keyword "[" |> parseLists <| keyword "]"

parseLists :: Parser Expr
parseLists = pure DottedList <*> es <*> keyword "." |> parseExpr
		 <|> pure List <*> es
	where 
		es = maybeSome parseExpr

-- parseList :: Parser Expr
-- parseList = pure List <*> maybeSome parseExpr

-- parseDottedList :: Parser Expr
-- parseDottedList = pure DottedList <*> head <*> tail
	-- where
		-- head = parseExpr <| keyword "."
		-- tail = parseExpr

parseQuoted :: Parser Expr
parseQuoted = pure List <*> es
	where
		es = pure (:) <*> pure (Atom "quote") <*> char '\'' |> exactlyOne parseExpr


showVal :: Expr -> String
showVal (Atom a) = a
showVal (Str s) = show s
showVal (Num n) = show n
showVal (Boo True) = "#t"
showVal (Boo False) = "#f"
showVal (List es) = "(" ++ unwordsList es ++ ")"
showVal (DottedList h t) = "(" ++ unwordsList h ++ " . " ++ showVal t ++ ")"


unwordsList :: [Expr] -> String
unwordsList = unwords . map showVal


readExpr :: String -> Expr
readExpr input = parse input


parse :: String -> Expr
parse s = case junk of
	""        -> exp
	otherwise -> error ("Parse error at '" ++ take 30 junk ++ "...'\n")
	where
		(junk, exp) = head $ parseExpr s


eval :: Expr -> Expr
eval (List [Atom "quote", e]) = e
eval (List [Atom "if", pred, conseq, alt]) = case eval pred of
    Boo False -> eval alt
    otherwise -> eval conseq
eval (List (Atom f : es) ) = apply f $ map eval es
eval e = e


apply :: String -> [Expr] -> Expr
apply f args = maybe (Boo False) ($ args) $ lookup f primitives


primitives :: [(String, [Expr] -> Expr)]
primitives = [  ("+", numericBinop (+) ),
				("-", numericBinop (-) ),
				("*", numericBinop (*) ),
				("/", numericBinop div ),
				("mod", numericBinop mod ),
				("quotient", numericBinop quot),
				("remainder", numericBinop rem),

				("=", numBoolBinop (==) ),
				("<", numBoolBinop (<)  ),
				(">", numBoolBinop (>)),
				("/=", numBoolBinop (/=)),
				(">=", numBoolBinop (>=)),
				("<=", numBoolBinop (<=)),

				("&&", booBoolBinop (&&)),
				("||", booBoolBinop (||)),

				("string=?", strBoolBinop (==)),
				("string<?", strBoolBinop (<)),
				("string>?", strBoolBinop (>)),
				("string<=?", strBoolBinop (<=)),
				("string>=?", strBoolBinop (>=)),
                
                ("head", car),
                ("tail", cdr),
                ("cons", cons),
                ("eq?", eqv),
                ("eqv?", eqv) ]
				--("", ),

numericBinop :: (Integer -> Integer -> Integer) -> [Expr] -> Expr
numericBinop op params = Num $ foldl1 op $ map unpackNum params

boolBinop :: (Expr -> a) -> (a -> a -> Bool) -> [Expr] -> Expr
boolBinop unpack op [a1, a2] = Boo $ op ( unpack a1 ) ( unpack a2 )
boolBinop _ _ _ = error "Binary function called with wrong number of args"

numBoolBinop = boolBinop unpackNum
strBoolBinop = boolBinop unpackStr
booBoolBinop = boolBinop unpackBool


unpackStr :: Expr -> String
unpackStr (Str s) = s
unpackStr (Num n) = show n
unpackStr (Boo b) = show b
unpackStr _ = ""

unpackBool :: Expr -> Bool
unpackBool (Boo b) = b
unpackBool _ = True

unpackNum (Num n) = n
unpackNum (Str n) = if null parsed then 0 else fst $ parsed !! 0
	where
		parsed = reads n
unpackNum (List [n]) = unpackNum n
unpackNum _ = 0

-- lisp head function
car :: [Expr] -> Expr
car [ List (x : xs) ] = x
car [ DottedList (x : xs) _] = x
car e = error $ "Cannot get the head of value " ++ show e

-- lisp tail function
cdr :: [Expr] -> Expr
cdr [ List (x : xs) ] = List xs
cdr [ DottedList [] e] = e
cdr [ DottedList (x : xs) e] = DottedList xs e
cdr e = error $ "Cannot get the tail of value " ++ show e

cons :: [Expr] -> Expr
cons [e, List []] = List [e]
cons [e, List es] = List (e:es)
cons [e, DottedList xs x] = DottedList (e:xs) x
cons [e, x] = DottedList [e] x
cons e = error $ "Cannot cons " ++ show e ++ " into a list."

eqv :: [Expr] -> Expr
eqv [Boo b1, Boo b2] = Boo $ b1 == b2
eqv [Num n1, Num n2] = Boo $ n1 == n2
eqv [Str s1, Str s2] = Boo $ s1 == s2
eqv [Atom a1, Atom a2] = Boo $ a1 == a2
eqv [DottedList xs x, DottedList ys y] = eqv [List (x:xs), List (y:ys)]
eqv [List xs, List ys] = Boo $ length xs == length ys && (all eqvPair $ zip xs ys)
    where
        eqvPair (a, b) = unpackBool $ eqv [a, b]
eqv [_, _] = Boo False
eqv _ = error "Binary comparisons require exactly two arguments"






{-

stringLiteral :: Parser String
stringLiteral =   token ( char '"'  |> maybeSome ( satisfy (/= '"') )  <| char '"' )
--              <|> token ( char '\'' |> maybeSome ( satisfy (/= '\'') ) <| char '\'' )

symbol :: Parser String
symbol = token $ pure (:) <*> symbolFirstChar <*> maybeSome symbolChar

number :: Parser Integer
number = token $ pure (read) <*> atLeastOne digit

boolean :: Parser Bool
boolean = token $ pure read <*> ( keyword "True" <|> keyword "False" )
	
expr :: Parser Expr
expr =  pure Boo <*> boolean
	<|> pure Num <*> number
	<|> pure Str <*> stringLiteral
	<|> pure List <*> keyword "[" |> maybeSome expr <| keyword "]"
--	<|> pure Pair <*> atLeastOne expr <| keyword "." <*> expr <| keyword ")"
	<|> pure Atom <*> symbol

parse :: String -> [Expr]
parse s = case junk of
	""        -> exps
	otherwise -> error ("Parse error at '" ++ take 30 junk ++ "...'\n")
	where
		(junk, exps) = head $ maybeSome expr s

eval :: Expr -> Expr
eval ( List (Atom f:args) ) = apply f $ map eval args
eval e = e

apply :: String -> [Expr] -> Expr
apply f args = maybe (Boo False) ($ args) $ lookup f primitives

primitives :: [(String, [Expr] -> Expr)]
primitives = [	("+", numericBinop (+) ),
				("-", numericBinop (-) ),
				("*", numericBinop (*) ),
				("/", numericBinop div ),

				("=",  boolBinop (==) ),
				("!=", boolBinop (/=) ),
				("<",  boolBinop (<)  ),
				(">",  boolBinop (>)  ),
				(">=", boolBinop (>=) ),
				("<=", boolBinop (<=) ),
		
				("mod", numericBinop mod ),
				("quotient", numericBinop quot ),
				("remainder", numericBinop rem ),
				("concat", concatStr) ]

concatStr :: [Expr] -> Expr
concatStr es = Str $ concat $ map show es
	where
		stringVal :: Expr -> String
		stringVal (Num n) = show n
		stringVal (Boo b) = show b
		stringVal (Str s) = s
		stringVal _ = ""


numericBinop :: (Integer -> Integer -> Integer) -> [Expr] -> Expr
numericBinop op params = Num $ foldl1 op $ map unpackNum params		

boolBinop :: (a -> a -> Bool) -> [Expr] -> Expr
boolBinop op [Num n1, Num n2] = Boo $ op n1 n2
boolBinop op [Boo b1, Boo b2] = Boo $ op b1 b2
boolBinop _ _ = Boo False

unpackNum :: Expr -> Integer
unpackNum (Num n) = n
unpackNum _ = 0

unpackBool :: Expr -> Bool
unpackBool (Boo n) = n
unpackBool _ = True

-}

flushString :: String -> IO ()
flushString s = putStr s >> hFlush stdout

prompt :: String -> IO String
prompt s = flushString s >> getLine

evalString :: String -> IO String
evalString s = return $ show $ eval $ readExpr s

evalAndPrint :: String -> IO ()
evalAndPrint s = evalString s >>= putStrLn

until_ :: (String -> Bool) -> IO String -> ( String -> IO () ) -> IO ()
until_ exitCond reader writer = do
    s <- reader
    case exitCond s of
        True -> return ()
        False -> writer s >> until_ exitCond reader writer


repl :: IO ()
repl = until_ (== "exit") (prompt "Lispish>>> ") evalAndPrint



main :: IO ()
main = getArgs >>= print . eval . readExpr . head



module HtmlGen.Syntax where


data Exp = Fun ( Library -> Exp -> (Library, Exp) )
		 | Sym Id
		 | Lit String
		 | Def Id Exp
		 | Lis [Exp]


type Id = String
type Library = [ Definition ]
type Definition = ( Id, Exp )


funDefine :: Library -> Exp -> (Library, Exp) 
funDefine l 

coreLibrary = [ ("::", funDefine ) ]


libraryLookup :: Library -> String -> Exp
libraryLookup l s = case lookup s l of 
	Just  e -> e
	Nothing -> error $ "Undefined function '" ++ s ++ "'\n"




{-
data Exp = Tag Id Exp
		 | Lit String
		 | Mac Id Arg
		 | Def Id Exp
		 | Att Attr
		 | Mul [Exp] -- multiple expressions
		 | Ref Id
		 | Pri Prim
	deriving Show

data Prim = TagId | Attrs | Content -- | SpacedContent | BlockContent


data Value = Val String
		   | Ref Id
	deriving Show


type Id = String
type Arg = String
type Attr = (Id, Value)
type Definition = (Id, Exp)
type Library = [ Definition ] -}




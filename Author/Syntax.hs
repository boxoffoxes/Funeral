module HtmlGen.Syntax where


data Exp = App Id Exp  -- fun arg
		 | Lit String  -- "string"
		 | Def Id Exp  -- key=value
		 | Sym Id      -- #key
		 | Lis [Exp]   -- [ e1 e2 e3 ]
	deriving Show


type Id = String
type Library = [ Definition ]
type Definition = ( Id, Exp )




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




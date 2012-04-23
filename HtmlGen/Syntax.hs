module HtmlGen.Syntax where


data Exp = Tag Id Exp
		 | Lit String
		 | Mac Id Arg
		 | Def Id Exp
		 | Att Attr
		 | Mul [Exp] -- multiple expressions
		 | Ref Id
		 | Pri Prim
	deriving Show

data Prim = TagId | Attrs | Content


data Value = Val String
		   | Ref Id
	deriving Show


type Id = String
type Arg = String
type Attr = (Id, Value)
type Definition = (Id, Exp)
type Library = [ Definition ]







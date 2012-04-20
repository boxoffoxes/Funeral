module HtmlGen.Syntax where


data Exp = Tag Id Exp
		 | Lit String
		 | Mac Id Arg
		 | Def Id Exp
		 | Att Attr
		 | Mul [Exp] -- multiple expressions
	deriving Show

type Id = String
type Arg = String
type Attr = (Id, String)
type Definition = (Id, Exp)
type Library = [ Definition ]







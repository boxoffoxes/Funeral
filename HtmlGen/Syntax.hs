module HtmlGen.Syntax where


data Exp = Tag Id Exp
		 | Lit String
		 | Mac Id Arg
		 | Def Id Exp
		 | Att Attr
		 | Mul [Exp] -- multiple expressions
		 | Opt String
	deriving Show -- TODO: add Comment syntax

data Value = Val String
		   | Ref String
	deriving Show


type Id = String
type Arg = String
type Attr = (Id, Value)
type Definition = (Id, Exp)
type Library = [ Definition ]







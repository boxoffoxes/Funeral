module HtmlGen.Syntax where


data Exp = Tag Id [Attr] [Exp]
		 | Lit String
		 | Mac Id Arg
		 | Def Id [Exp]
	deriving Show

type Id = String
type Arg = String
type Attr = (Id, String)
type Definition = (Id, [Exp])
type Library = [ Definition ]







module Author.Syntax where

data Exp = App Id Exp  -- fun arg
         | Num Integer -- 1
         | Str String  -- "string" or 'string' or `string`
         | Att Id Exp  -- key=value
         | Ref Id      -- $key
         | Lis [Exp]   -- [ e1 e2 e3 ]
		 | Fun (Exp -> Exp)
--    deriving Show

instance Show Exp where 
	show (App id exp) = "App " ++ show id ++ " " ++ show exp
	show (Num n) = "Num " ++ show n
	show (Str s) = "Str " ++ show s
	show (Att id e) = "Att " ++ show id ++ " " ++ show e
	show (Lis es) = "Lis " ++ show es
	show (Ref r) = "Ref " ++ show r
	show (Fun f) = "<function>"
	

type Id = String
type Library = [ Definition ]
type Definition = ( Id, Exp )



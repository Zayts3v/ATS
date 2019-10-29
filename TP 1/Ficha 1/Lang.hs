import Parser
import Data.Char

import Prelude hiding ((<*>),(<$>))

number =  f <$> satisfy isDigit
      <|> g <$> satisfy isDigit <*> number
    where f a    = [a]
          g a b = a:b

ident = oneOrMore (satisfy isAlpha)

data Exp = AddExp Exp Exp
         | MulExp Exp Exp
         | SubExp Exp Exp
         | DivExp Exp Exp
         | GThen Exp Exp
         | LThen Exp Exp
         | OneExp Exp
         | Var String
         | Const Int
         | SpaceExp String

instance Show Exp where
    show = showExp

showExp (AddExp e1 e2) = showExp e1 ++ "+" ++ showExp e2
showExp (SubExp e1 e2) = showExp e1 ++ "-" ++ showExp e2
showExp (MulExp e1 e2) = showExp e1 ++ "*" ++ showExp e2
showExp (DivExp e1 e2) = showExp e1 ++ "/" ++ showExp e2
showExp (GThen e1 e2)  = showExp e1 ++ ">" ++ showExp e2
showExp (OneExp e)     = "(" ++ showExp e ++ ")"
showExp (Const i)      = show i
showExp (Var a)        = a

pexp :: Parser Char Exp
pexp  =  f <$> pterm
     <|> g <$> pterm <*> symbol '+' <*> pexp
     <|> h <$> pterm <*> symbol '-' <*> pexp
    where f a     = a
          g a b c = AddExp a c
          h a b c = SubExp a c

pterm :: Parser Char Exp
pterm =  f <$> pfactor
     <|> g <$> pfactor <*> symbol '*' <*> pterm
     <|> h <$> pfactor <*> symbol '/' <*> pterm
    where f a     = a
          g a b c = MulExp a c
          h a b c = DivExp a c

pfactor :: Parser Char Exp
pfactor =  f <$> number
       <|> g <$> ident
       <|> i <$> symbol '(' <*> pexp <*> symbol ')'
    where f a = Const (read a)
          g a = Var a
          i a e f = OneExp e


module Parser where 

import Data.Char
import Prelude hiding ((<*>),(<$>))
import Lib

data P = R Its

type Its = [It]

data It = Block Its
        | Decl String
        | Use String

instance Show It where
    show = showIt

showIt (Block its) = "[" ++ (printIts its) ++ "]" 
showIt (Decl s)    = "Decl " ++ s
showIt (Use s)     = "Use " ++ s

printIts :: [It] -> String
printIts []   = ""
printIts (h:[])
    | ((token "Decl" (show h)) == []) = (showIt h)
    | ((token "Use"  (show h)) == []) = (showIt h)
    | otherwise             = ""
printIts (h:t)
    | ((token "Decl" (show h)) == []) = (showIt h) ++ "," ++ (printIts t)
    | ((token "Use"  (show h)) == []) = (showIt h) ++ "," ++ (printIts t)
    | otherwise             = "," ++ (printIts t)

{-- 
pp_P (R its) = "[" ++ pp_Its its ++ "]"

pp_Its [] = ""
pp_Its [it] = pp_It it
pp_Its (it:its) = pp_It it ++ " , " ++ pp_Its its

pp_It (Decl n) = "Decl " ++ n
pp_It (Use n) = "Use " ++ n
pp_It (Block is) = "[" ++ pp_Its is ++ "]"
-}

-- Fazer com enclosedBy quando a solução estiver implementada na ficha anterior
pMain :: Parser Char P
pMain = f <$> (enclosedBy (symbol' '[') pBlock (symbol' ']'))
    where f a = R a

pBlock :: Parser Char Its
pBlock =  f <$> token ""
      <|> g <$> symbol' '[' <*> pBlock <*> symbol' ']'  
      <|> h <$> token ""
    where f a = []
          g a b c = b
          h a = []

pStatment :: Parser Char It
pStatment a = []


-- Lista de Use, Lista de Decl, Lista de Erros.
-- Algumas delas vão dar reset no block, outras no statment

{--______________________TESTING________________________-}

-- input = "[ Use "y" , Decl "x" , [ Decl "y" , Use "x" , Decl "y" ] , Use "x" ]"

-- Output should be like this:
    -- [Use "y",Decl "y"]


-- teste output
use1 = Use "y"
decl = Decl "x"
lista = [use1, decl]

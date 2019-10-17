module Parser where 

import Data.Char
import Data.String
import Lib

import Prelude hiding ((<*>),(<$>))

data P = R Its

type Its = [It]

data It = Block Its
        | Decl String
        | Use String

instance Show P where
    show = showP

instance Show It where
    show = showIt

instance Eq It where
    Use  x == Use y = x == y
    Decl x == Use y = x == y

showP (R its) = "[" ++ (printIts its) ++ "]"

showIt (Block its) = "["     ++ (printIts its) ++ "]" 
showIt (Decl s)    = "Decl " ++ s
showIt (Use s)     = "Use "  ++ s

printIts :: [It] -> String
printIts []   = ""
printIts (h:[])
    | ((token "Decl" (show h)) == []) = (showIt h)
    | ((token "Use"  (show h)) == []) = (showIt h)
    | otherwise                       = ""
printIts (h:t)
    | ((token "Decl" (show h)) == []) = (showIt h) ++ "," ++ (printIts t)
    | ((token "Use"  (show h)) == []) = (showIt h) ++ "," ++ (printIts t)
    | otherwise                       = "," ++ (printIts t)

pMain :: Parser Char P
pMain = f <$> (symbol' '[') <*> pBlock <*> (symbol' ']')
    where f a b c = R b

pBlock :: Parser Char Its
pBlock =  f <$> token " "
      <|> g <$> pStatment                                                   <*> pBlock
      <|> h <$> (separatedBy pStatment (token' " , "))                      <*> pBlock
      <|> i <$> (token' " [ ")   <*> (separatedBy pStatment (token' " , ")) <*> (token' " ] ")   <*> pBlock
      <|> j <$> (token' " [ ")   <*> (separatedBy pStatment (token' " , ")) <*> (token' " ] , ") <*> pBlock
      <|> k <$> (token' " , [ ") <*> (separatedBy pStatment (token' " , ")) <*> (token' " ] ")   <*> pBlock
      <|> l <$> (token' " , [ ") <*> (separatedBy pStatment (token' " , ")) <*> (token' " ] , ") <*> pBlock
    where f a       = []
          g a b     = a:b
          h a b     = a++b
          i a b c d = b++d
          j a b c d = b++d
          k a b c d = b++d
          l a b c d = b++d

pStatment :: Parser Char It
pStatment =  f <$> pIt <*> ident
        where f a b = if (a=="Use ")
                      then Use b
                      else Decl b

pIt :: Parser Char [Char]
pIt =  f <$> token' "Use "
   <|> g <$> token' "Decl "
    where f a = a
          g a = a

-- Lista de Use, Lista de Decl, Lista de Erros.
-- Algumas delas vão dar reset no block, outras no statment

{--______________________TESTING________________________-}

-- input = "[ Use y, Decl x, [ Decl y , Use x , Decl y ] , Use x ]"

-- Output should be like this:
    -- "[Use y,Decl y]"

-- teste output
usey = Use "y"
decx = Decl "x"
decy = Decl "y"
usex = Use "x"
listaSub = [decy, usex, decy]
blockEx = Block listaSub
lista = [usey, decx, blockEx, usex]
output = R lista

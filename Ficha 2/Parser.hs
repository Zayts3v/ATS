module Parser where 

import Data.Char
import Data.String
import Data.List
import Lib

import Prelude hiding ((<*>),(<$>))

data P = R Its

type Its = [It]

data It = Block Its
        | Decl String
        | Use String

type Outs = [(It,Int)]

type Out = (It,Int)

instance Show P where
    show = showP

instance Show It where
    show = showIt

instance Eq It where
    Decl x == Use y  = (False == True)
    Use  x == Decl y = (False == True)
    Use  x == Use y  = (x == y)
    Decl x == Decl y = (x == y)

showP (R its)   = "[" ++ printIts its ++ "]"

showIt (Decl s) = "Decl " ++ s
showIt (Use s)  = "Use "  ++ s

printIts :: Its -> String
printIts []     = ""
printIts (h:[]) = (showIt h) 
printIts (h:t)  = (showIt h) ++ "," ++ (printIts t)

-- Parser
pMain :: Parser Char P
pMain =   f <$> (symbol' '[') <*> pBlock 1 <*> (symbol' ']')
    where f a b c = R (extract b)

pBlock :: Int -> Parser Char Outs
pBlock nivel =  l <$> succeed []
            <|> n <$> (separatedBy (pStatment nivel) (token' " , "))                      <*> (pBlock (nivel+1))
            <|> o <$> (token' " [ ")   <*> (separatedBy (pStatment nivel) (token' " , ")) <*> (pBlock (nivel+1)) <*> (token' " ] ")
            <|> p <$> (token' " [ ")   <*> (separatedBy (pStatment nivel) (token' " , ")) <*> (pBlock (nivel+1)) <*> (token' " ] , ") <*> (pBlock (nivel-1))
            <|> q <$> (token' " , [ ") <*> (separatedBy (pStatment nivel) (token' " , ")) <*> (pBlock (nivel+1)) <*> (token' " ] ")
            <|> r <$> (token' " , [ ") <*> (separatedBy (pStatment nivel) (token' " , ")) <*> (pBlock (nivel+1)) <*> (token' " ] , ") <*> (pBlock (nivel-1))
        where l a         = a
              n a b       =  a++b
              o a b c d   =  b++c
              p a b c d e = (b++c) ++ e
              q a b c d   =  b++c
              r a b c d e = (b++c) ++ e

pStatment :: Int -> Parser Char Out
pStatment nivel = f <$> pIt <*> ident
            where f a b = if (a=="Use ")
                          then (Use b, nivel)
                          else (Decl b, nivel)

pIt :: Parser Char [Char]
pIt =  f <$> token' "Use "
   <|> g <$> token' "Decl "
    where f a = a
          g a = a

-- Tratamento
extract :: Outs -> Its
extract []    = []
extract lista = extractAux lista lista

extractAux :: Outs -> Outs -> Its
extractAux [] _ = []
extractAux ((x,y):t) lista 
    | (((take 3 (show x)) == "Use")  && (uses  x y lista))          = x:(extractAux t lista)
    | (((take 4 (show x)) == "Decl") && ((decs x y lista 0) > 1))   = x:(extractAux (remove (x,y) t) lista)
    | otherwise                                                     = (extractAux t lista)

uses :: It -> Int -> Outs -> Bool
uses _ _ []            = True
uses i nivel ((x,y):t) =
    if ((nivel >= y) && ((take 4 (show x)) == "Decl") && ((drop 5 (show x))==(drop 4 (show i))))
    then False
    else (uses i nivel t)

decs :: It -> Int -> Outs -> Int -> Int
decs _ _ [] number            = number
decs i nivel ((x,y):t) number =
    if ((nivel == y) && ((take 4 (show x)) == "Decl") && ((drop 5 (show x))==(drop 5 (show i))))
    then (decs i nivel t (number + 1))
    else (decs i nivel t number)

remove :: Out -> Outs -> Outs
remove _ []            = []
remove (a,b) ((x,y):t) =
    if ((a==x) && (b==y))
    then (remove (a,b) t)
    else (x,y):(remove (a,b) t)

{--_____________________________________________FOR TEST________________________-}

-- inputOfical = "[ Use y , Decl x , [ Decl y , Use x , [ Decl x , Use d ] , Decl y ] , Use x]"
-- Output should be like this: "[Use y,Decl y,Use d]"

-- teste de It
usey = Use "y"
decx = Decl "x"
decy = Decl "y"
usex = Use "x"
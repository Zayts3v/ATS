module Ficha1 where

import Prelude hiding ((<*>),(<$>))

import Data.Char
import Parser hiding (oneOrMore, zeroOrMore, spaces, symbol')

data Exp = AddExp Exp Exp
         | MulExp Exp Exp
         | SubExp Exp Exp
         | GThen Exp Exp
         | LThen Exp Exp
         | OneExp Exp
         | Var String
         | Const Int

instance Show Exp where
  show = showExp

showExp (AddExp e1 e2) = showExp e1 ++ " + " ++ showExp e2
showExp (SubExp e1 e2) = showExp e1 ++ " - " ++ showExp e2
showExp (MulExp e1 e2) = showExp e1 ++ " * " ++ showExp e2
showExp (GThen e1 e2)  = showExp e1 ++ " > " ++ showExp e2
showExp (OneExp e)     = "( " ++ showExp e ++ " )"
showExp (Const i)      = show i
showExp (Var a)        = a 

-- | 1.1
number  =   f <$> satisfy isDigit
       <|>  g <$> satisfy isDigit <*> number
  where f a = [a]
        g a b = a:b

ident =  oneOrMore (satisfy isAlpha)

-- | 1.2
e = MulExp (OneExp (AddExp (Var "var") (Const 3))) (Const 5)

-- | 1.3
pexp :: Parser Char Exp
pexp =   f <$> pterm
     <|> g <$> pterm <*> symbol' '+' <*> pexp
     <|> h <$> pterm <*> symbol' '-' <*> pexp
   where f a = a
         g a b c = AddExp a c
         h a b c = SubExp a c

pterm :: Parser Char Exp
pterm =  f <$> pfactor
     <|> g <$> pfactor <*> symbol' '*' <*> pterm
   where f a = a
         g a b c = MulExp a c

pfactor :: Parser Char Exp
pfactor =   f <$> number
       <|>  g <$> ident
       <|>  h <$> symbol' '(' <*> pexp <*> symbol' ')'
  where f a = Const (read a)
        g a = Var a
        h a e f = OneExp e

-- | 1.4
symbol' a = (\ a b c -> b) <$> spaces <*> symbol a <*> spaces

-- | 1.5
zeroOrMore :: Parser s r -> Parser s [r]
zeroOrMore p = sf <$> p <*> zeroOrMore p
             <|> succeed []
  where sf x xs = x : xs

-- | 1.6
spaces = zeroOrMore (satisfy (\x -> x `elem` [' ','\t','\n']))

-- | 1.7

data Prog = Prog Stats

type Id = String

data Stats = Stats [Stat]

data Stat = While Exp Stats
          | IfThenElse Exp Stats Stats
          | Assign Id Exp

instance Show Prog where
  show = showProg

showProg (Prog sts) = showStats sts
  
instance Show Stats where
  show = showStats

showStats (Stats l) = showStatsList l

showStatsList []       = ""
showStatsList (st:[])  = showStat st
showStatsList (st:sts) = showStat st ++ ";\n " ++ (showStatsList sts)

instance Show Stat where
  show = showStat

showStat (Assign n e)            = n ++ " = " ++ showExp e
showStat (While e sts)           = "While (" ++ showExp e ++ ")\n " ++ "{ " ++ showStats sts ++ "}" 
showStat (IfThenElse e sts stsS) = "If (" ++ showExp e ++ ")\nthen { " ++ showStats sts ++ "}\nelse { " ++ showStats stsS ++ "}"

-- 1.7 - Resolução
oneOrMore p =  sf1 <$> p <*> zeroOrMore p
  where sf1 x xs = x : xs
        sf2 x = [x]

-- 1.8
pProg :: Parser Char Prog
pProg = f <$> pStats
    where f a = Prog a

pStats :: Parser Char Stats
pStats =  f <$> token ""
      <|> g <$> pStat
      <|> h <$> pStat <*> token ";\n" <*> pStats
  where f a     = Stats []
        g a     = Stats [a]
        h a b c = Stats (a:(unStats c)) 

unStats :: Stats -> [Stat]
unStats (Stats b) = b

pStat :: Parser Char Stat
pStat =  f  <$> ident           <*> symbol' '=' <*> pexp
      <|> g <$> token "while (" <*> pexp        <*> token ")\n"      <*> token "{ " <*> pStats           <*> symbol' '}'
      <|> h <$> token "if ("    <*> pexp        <*> token ")\nthen {" <*> pStats     <*> token "}\nelse{" <*> pStats <*> symbol' '}'
  where f a b c         = Assign a c
        g a b c d e f   = While b e
        h a b c d e f g = IfThenElse b d f

{-
  Exercício 1.9) No desenvolvimento do parser pProg foram utilizadas construções sintáticas
    muito frequentes em linguagem de programação: separatedBy (lista de elementos sepa-
    rados por um dado separador, neste exemplo ponto e virgula), enclosedBy (elementos
    delimitados por um s ́ımbolo inicial e final, neste exemplo parentesis curvos). Defina em
    Parser.hs estes combinadores que descartam o resultado de fazer parsing aos separado-
    res/delimitadores.
-}

--separatedBy :: Parser s a -> Parser s b -> Parser s [a]
--enclosedBy :: Parser s a -> Parser s b -> Parser s c -> parser s b

{-
  Exercício 1.10) Re-escreva pProg utilizando separatedBy e enclosedBy
-}

{-
  Exercício 1.11) Adicione à biblioteca Parser.hs mais construções sintáticas frequentes
    em longuagens de programação, nomeadamente:
-}

{-
followedBy :: Parser s a -> Parser s b -> Parser s [a]
block :: Parser s a -- open delimiter
-> Parser s b -- syntactic symbol that follows statements
-> Parser s r -- parser of statements
-> Parser s f -- close delimiter
-> Parser s [r]
-}

progA = "i = 1+2"
progB = "while (1+2) { i= i + 10 }"
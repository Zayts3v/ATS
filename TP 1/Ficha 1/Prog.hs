module Ficha1 where

import Prelude hiding ((<*>),(<$>))

import Data.Char
import Parser hiding (oneOrMore, zeroOrMore, spaces, symbol', enclosedBy, separatedBy, followedBy, block)

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
         h a b c = SubExp a Char

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
pStat =  f <$> ident            <*> symbol' '=' <*> pexp
     <|> g <$> token' "while (" <*> pexp        <*> token' ")\n"       <*> token' "{"  <*> pStats            <*> symbol' '}'
     <|> h <$> token' "if ("    <*> pexp        <*> token' ")\nthen {" <*> pStats      <*> token' "}\nelse{" <*> pStats <*> symbol' '}'
    where f a b c         = Assign a c
          g a b c d e f   = While b e
          h a b c d e f g = IfThenElse b d f

-- | testing 1.8
progA = "  while ( i+2 )\n  { i = 5 + 5 }"
progB = "  if ( i + 0 )\nthen { i = i + 0 }\nelse{ i = i + 0 }"

-- | 1.9
separatedBy :: Parser s a -> Parser s b -> Parser s [a]
separatedBy d s =  f <$> d
               <|> g <$> d <*> s <*> separatedBy d s
            where f a     = [a]
                  g a b c = a:c

enclosedBy :: Parser s a -> Parser s b -> Parser s c -> Parser s b
enclosedBy a b c = f <$> a <*> b <*> c
        where f a b c = b

-- | 1.10
pProgV2 :: Parser Char Prog
pProgV2 = f <$> pStats
    where f a = Prog a

pStatsV2 :: Parser Char Stats
pStatsV2 = f <$> separatedBy pStat (token' ";")
    where f a = Stats a

pStatV2 :: Parser Char Stat
pStatV2 =  f <$> ident          <*> symbol' '=' <*> pexp
       <|> g <$> token' "while" <*> (enclosedBy (symbol' '(') pexp (symbol' ')'))
                                <*> (enclosedBy (symbol' '{') pStats (symbol' '}'))
       <|> h <$> token' "if"    <*> (enclosedBy (symbol' '(') pexp (symbol' ')'))
                                <*> token' "then" <*> (enclosedBy (symbol' '{') pStats (symbol' '}'))
                                <*> token' "else" <*> (enclosedBy (symbol' '{') pStats (symbol' '}'))
    where f a b c       = Assign a c
          g a b c       = While b c
          h a b c d e f = IfThenElse b d f

-- | testing
progC = "i = 4 + 0; if ( i + 0 )\nthen { i = i + 0 }\nelse{ i = i + 0 }"
-- writeFile "output.txt" (show (pProgV2 progC))

-- | 1.11
followedBy :: Parser s a -> Parser s b -> Parser s [a]
followedBy a b =  f <$> a <*> b
              <|> g <$> a <*> b <*> followedBy a b
        where f a b   = [a]
              g a b c = a:c

block :: Parser s a -> Parser s b-> Parser s r -> Parser s f -> Parser s [r]
block a b c d = enclosedBy a (followedBy c b) d
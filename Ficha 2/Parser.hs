data P = R Its

type Its = [It]

data It = Block Its
        | Decl String
        | Use String

pp_P (R its) = "[" ++ pp_Its its ++ "]"

pp_Its [] = ""
pp_Its [it] = pp_It it
pp_Its (it:its) = pp_It it ++ " , " ++ pp_Its its

pp_It (Decl n) = "Decl " ++ n
pp_It (Use n) = "Use " ++ n
pp_It (Block is) = "[" ++ pp_Its is ++ "]"








pMain :: Parser Char P
pMain = f <$> symbol' '[' <*> pBlock <*> symbol' ']'
    where f a = R a

pBlock :: Parser Char Its
pBlock =  f <$> token ""
  --    <|> g <$>  
    where f a = [] 

pStatment :: Parser Char It
pStatment a = []
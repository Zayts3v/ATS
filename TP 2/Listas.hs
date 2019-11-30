module Listas (
    listaLocalidades,
    listaPreferencias
) where

listaLocalidades :: [String]
listaLocalidades = ["Agueda"  ,"Albufeira","Almeirim" ,"Baiao"   ,"Barreiro"   ,"Braga"  ,
                    "Cartaxo" ,"Chaves"   ,"Coimbra"  ,"Elvas"   ,"Espinho"    ,"Faro"   ,
                    "Funchal" ,"Gois"     ,"Guarda"   ,"Lagos"   ,"Lamego"     ,"Trofa"  ,
                    "Melgaco" ,"Moita"    ,"Mogadouro","Nelas"   ,"Obidos"     ,"Tomar"  ,
                    "Odivelas","Ourem"    ,"Pombal"   ,"Porto"   ,"Penacova"   ,"Tabua"  ,
                    "Sabugal" ,"Sabrosa"  ,"Resende"  ,"Santarem","Santo Tirso","Seia"   ,
                    "Seixal"  ,"Sesimbra" ,"Serta"    ,"Sintra"  ,"Tabuaco"    ,"Tondela",
                    "Tavira"  ,"Vagos"    ,"Valenca"  ,"Valpacos","Baiao"      ,"Valongo",
                    "Viseu"   ,"Vouzela"  ,"Vila Real","Vizela"  ,"Tarouca"    ,"Vinhais",
                    "Vieira do Minho"     , "Torres Vedras"      ,"Vila Flor"  ,"Vale de Cambra"]

listaPreferencias :: [String]
listaPreferencias = ["MaisBarato","MaisPerto"]

{--
[(1,return "Agueda")  ,(1,return "Albufeira") ,(1,return "Almeirim"),
                       (1,return "Baiao")   ,(1,return "Barreiro")  ,(1,return "Braga"),
                       (1,return "Cartaxo") ,(1,return "Chaves")    ,(1,return "Coimbra"),
                       (1,return "Elvas")   ,(1,return "Espinho")   ,(1,return "Faro"),
                       (1,return "Funchal") ,(1,return "Gois")      ,(1,return "Guarda"),
                       (1,return "Lagos")   ,(1,return "Lamego")    ,(1,return "Melgaco"),
                       (1,return "Moita")   ,(1,return "Mogadouro") ,(1,return "Nelas"),
                       (1,return "Obidos")  ,(1,return "Odivelas")  ,(1,return "Ourem"),
                       (1,return "Pombal")  ,(1,return "Porto")     ,(1,return "Penacova"),
                       (1,return "Sabugal") ,(1,return "Resende")   ,(1,return "Sabrosa"),
                       (1,return "Resende") ,(1,return "Santarem")  ,(1,return "Santo Tirso"),
                       (1,return "Seia")    ,(1,return "Seixal")    ,(1,return "Sesimbra"),
                       (1,return "Serta")   ,(1,return "Sintra")    ,(1,return "Tabuaco"),
                       (1,return "Tondela") ,(1,return "Tomar")     ,(1,return "Tabua"),
                       (1,return "Tavira")  ,(1,return "Trofa")     ,(1,return "Vagos"),
                       (1,return "Valenca") ,(1,return "Valpacos")  ,(1,return "Baiao"),
                       (1,return "Valongo") ,(1,return "Viseu")     ,(1,return "Vieira do Minho"),
                       (1,return "Vizela")  ,(1,return "Vinhais")   ,(1,return "Torres Vedras"),
                       (1,return "Vouzela") ,(1,return "Vila Real") ,(1,return "Vale de Cambra"),
                       (1,return "Tarouca") ,(1,return "Vila Flor")] -}
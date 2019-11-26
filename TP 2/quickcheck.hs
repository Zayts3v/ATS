import Test.QuickCheck

import Data.List

data Carro =    NovoCarro Tipo Marca Matricula NIF VelMedia PPKm CPKm Autonomia X Y
                deriving Show
data Tipo =     Gasolina
                | Electrico
                | Hibrido
                deriving Show

type Marca      = String
type Matricula  = String
type NIF        = Int -- NIF Proprietario
type VelMedia   = Int
type PPKm       = Float
type CPKm       = Float -- Consumo por Km
type Autonomia  = Int
type X          = Float
type Y          = Float

genTipo :: Gen Tipo
genTipo = frequency [(42091,return Gasolina),(1217,return Hibrido),(472,return Electrico)]

genMarca :: Gen Marca
genMarca = frequency [(84,return "Abarth"),(15,return "Aixam"),(376,return "Alfa Romeo"),(3,return "Alpine"),(19,return "Aston Martin"),(2470,return "Audi"),(4,return "Austin"),(2,return "Bellier"),(32,return "Bentley"),(5033,return "BMW"),(9,return "Cadillac"),(169,return "Chevrolet"),(43,return "Chrysler"),(1728,return "Citroën"),(150,return "Dacia"),(9,return "Daewoo"),(10,return "Datsun"),(38,return "Dodge"),(116,return "DS"),(60,return "Ferrari"),(1949,return "Fiat"),
                        (1686,return "Ford"),(310,return "Honda"),(12,return "Hummer"),(390,return "Hyundai"),(7,return "Infiniti"),(4,return "Isuzu"),(264,return "Jaguar"),(172,return "Jeep"),(337,return "Kia"),(11,return "Lamborghini"),(68,return "Lancia"),(324,return "Land Rover"),(153,return "Lexus"),(9,return "Lotus"),(38,return "Maserati"),(392,return "Mazda"),(2,return "McLaren"),(4837,return "Mercedes-Benz"),(18,return "MG"),(7,return "Microcar"),(1069,return "MINI"),
                        (390,return "Mitsubishi"),(3,return "Morgan"),(1357,return "Nissan"),(2049,return "Opel"),(3492,return "Peugeot"),(2,return "Pontiac"),(637,return "Porsche"),(4874,return "Renault"),(3,return "Rolls Royce"),(14,return "Rover"),(24,return "Saab"),(1842,return "SEAT"),(466,return "Skoda"),(691,return "Smart"),(24,return "Subaru"),(83,return "Suzuki"),(47,return "Tesla"),(1130,return "Toyota"),(5,return "Triumph"),(1316,return "Volvo"),(2919,return "VW")]

genMatricula :: Gen Matricula
genMatricula = do l1 <- elements ['A'..'Z']
                  l2 <- elements ['A'..'Z']
                  n1 <- elements ['0'..'9']
                  n2 <- elements ['0'..'9']
                  n3 <- elements ['0'..'9']
                  n4 <- elements ['0'..'9']
                  return ((l1:l2:"-") ++ (n1:n2:"-") ++ (n3:n4:""))

genNIF :: Gen NIF
genNIF = choose (000000000,999999999)

genVelMedia :: Gen VelMedia
genVelMedia = elements [10..300]

genPPKm :: Gen PPKm
genPPKm = choose (1.0,3)

genCPKm :: Gen CPKm
genCPKm = choose (0.1,3)

genAutonomia :: Gen Autonomia
genAutonomia = elements [0..1000]

genX :: Gen X
genX = choose (-100.0,100)

genY :: Gen Y
genY = choose (-100.0,100)

genCarro :: Gen Carro
genCarro =  do  k <- genTipo
                i <- genMarca
                j <- genMatricula
                l <- genNIF
                m <- genVelMedia
                n <- genPPKm
                o <- genCPKm
                p <- genAutonomia
                q <- genX
                r <- genY
                return (NovoCarro k i j l m n o p q r)






































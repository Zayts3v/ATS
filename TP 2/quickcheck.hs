import Test.QuickCheck

import Data.List

data Carro =    NovoCarro Tipo Marca Matricula NIF VelMedia PPKm CPKm Autonomia X Y
                deriving Show
data Tipo =     Gasolina
                | Electrico
                | Hibrido
                deriving Show

data Cliente = Cliente Nome NIF Email Morada X Y
	deriving Show

data Proprietario = Proprietario Nome NIF Email Morada
	deriving Show

type Nome       = String
type Email 	= String
type Morada	= String
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

genEmail :: Gen Email
genEmail = return strCat (show (choose (000000000,999999999), ++ "@gmail.com"))

genNome :: Gen Nome
genNome = frequency [(4809,return "Maria"),(2062,return "Matilde"),(1859,return "Leonor"),(1378,return "Beatriz"),(1330,return "Mariana"),(1295,return "Carolina"),(1120,return "Ana"),(1062,return "Inês"),(980,return "Sofia"),(930,return "Margarida"),(929,return "Lara"),(708,return "Joana"),(651,return "Laura"),(650,return "Francisca"),(573,return "Diana"),(517,return "Mafalda"),(511,return "Madalena"),(504,return "Clara"),(500,return "Luana"),(488,return "Sara"),(482,return "Bianca"),(461,return "Alice"),(444,return "Rita"),(350,return "Eva"),(340,return "Gabriela"),(326,return "Camila"),(315,return "Yara"),(313,return "Benedita"),(312,return "Mara"),(295,return "Catarina"),(278,return "Ariana"),(244,return "Ema"),(243,return "Vitória"),(242,return "Marta"),(230,return "Carlota"),(1809,return "João"),(1783,return "Rodrigo"),(1718,return "Francisco"),(1663,return "Martim"),(1428,return "Santiago"),(1400,return "Tomás"),(1378,return "Afonso"),(1244,return "Duarte"),(1207,return "Miguel"),(1206,return "Guilherme"),(1140,return "Tiago"),(1133,return "Gonçalo"),(1129,return "Diogo"),(1128,return "Gabriel"),(1061,return "Pedro"),(993,return "Rafael"),(993,return "Salvador"),(810,return "Dinis"),(772,return "Lucas"),(720,return "Simão"),(706,return "Gustavo"),(689,return "David"),(683,return "José"),(648,return "Vicente"),(622,return "Lourenço"),(595,return "Diego"),(542,return "Daniel"),(519,return "António"),(492,return "André"),(490,return "Vasco"),(455,return "Manuel"),(433,return "Henrique"),(383,return "Leonardo"),(378,return "Bernardo")]

genMorada :: Gen Morada
genMorada = frequency [(1,return "Águeda"),(1,return "Albufeira"),(1,return "Almeirim"),(1,return "Baião"),(1,return "Barreiro"),(1,return "Braga"),(1,return "Cartaxo"),(1,return "Chaves"),(1,return "Coimbra"),(1,return "Elvas"),(1,return "Espinho"),(1,return "Faro"),(1,return "Funchal"),(1,return "Góis"),(1,return "Guarda"),(1,return "Lagos"),(1,return "Lamego"),(1,return "Melgaço"),(1,return "Moita"),(1,return "Mogadouro"),(1,return "Nelas"),(1,return "Óbidos"),(1,return "Odivelas"),(1,return "Ourém"),(1,return "Pombal"),(1,return "Porto"),(1,return "Penacova"),(1,return "Sabugal"),(1,return "Resende"),(1,return "Sabrosa"),(1,return "Resende"),(1,return "Santarém"),(1,return "Santo Tirso"),(1,return "Seia"),(1,return "Seixal"),(1,return "Sesimbra"),(1,return "Sertã"),(1,return "Sintra"),(1,return "Tabuaço"),(1,return "Tondela"),(1,return "Tomar"),(1,return "Tábua"),(1,return "Tavira"),(1,return "Trofa"),(1,return "Vagos"),(1,return "Valença"),(1,return "Valpaços"),(1,return "Vieira do Minho"),(1,return "Vila Real"),(1,return "Baião"),(1,return "Vinhais"),(1,return "Viseu"),(1,return "Vouzela"),(1,return "Vizela"),(1,return "Vila Flor"),(1,return "Tarouca"),(1,return "Torres Vedras"),(1,return "Vale de Cambra"),(1,return "Valongo")]


genCliente :: Gen Cliente
genCliente = do m <- genNome
		n <- genNIF
		o <- genEmail
		p <- genMorada
		q <- genX
		r <- genY
		return (Cliente m n o p q r)

genProprietario :: Gen Proprietario
genProprietario = do m <- genNome
		     n <- genNIF
		     o <- genEmail
		     p <- genMorada
		     return (Proprietario m n o p)











































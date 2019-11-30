import Test.QuickCheck

import Data.List
import Listas

data Carro = NovoCarro Tipo Marca Matricula NIF VelMedia PPKm CPKm Autonomia X Y
                deriving Show

data Tipo = Gasolina
          | Electrico
          | Hibrido
            deriving Show

data Cliente = Cliente Nome NIF Email Morada X Y
    deriving Show

data Proprietario = Proprietario Nome NIF Email Morada
    deriving Show

data Aluguer = Aluguer NifCliente X Y Tipo Preferencia
    deriving Show

data Classificar = Classificar ClassProp Nota
    deriving Show

type Marca       = String
type Matricula   = String
type NIF         = Int
type VelMedia    = Int
type PPKm        = Float
type CPKm        = Float
type Autonomia   = Int
type X           = Float
type Y           = Float
type Nome        = String
type Email       = String
type Morada      = String
type NifCliente  = String
type Preferencia = String
type ClassProp   = String
type Nota        = Int

genTipo :: Gen Tipo
genTipo = frequency [(42091,return Gasolina),(1217,return Hibrido),(472,return Electrico)]

genMarca :: Gen Marca
genMarca = frequency [(84  ,return "Abarth")  ,(15  ,return "Aixam")   ,(376 ,return "Alfa Romeo"),
                      (3   ,return "Alpine")  ,(32,return "Bentley")   ,(2470,return "Audi"),
                      (4   ,return "Austin")  ,(2   ,return "Bellier") ,(19  ,return "Aston Martin"),
                      (5033,return "BMW")     ,(9   ,return "Cadillac"),(169 ,return "Chevrolet"),
                      (43  ,return "Chrysler"),(1728,return "Citroën") ,(150 ,return "Dacia"),
                      (9   ,return "Daewoo")  ,(10  ,return "Datsun")  ,(38  ,return "Dodge"),
                      (116 ,return "DS")      ,(60  ,return "Ferrari") ,(1949,return "Fiat"),
                      (1686,return "Ford")    ,(310 ,return "Honda")   ,(12  ,return "Hummer"),
                      (390 ,return "Hyundai") ,(7   ,return "Infiniti"),(4   ,return "Isuzu"),
                      (264 ,return "Jaguar")  ,(172 ,return "Jeep")    ,(337 ,return "Kia"),
                      (1069,return "MINI")    ,(68  ,return "Lancia")  ,(324 ,return "Land Rover"),
                      (153 ,return "Lexus")   ,(9   ,return "Lotus")   ,(38  ,return "Maserati"),
                      (392 ,return "Mazda")   ,(2   ,return "McLaren") ,(4837,return "Mercedes-Benz"),
                      (18  ,return "MG")      ,(7   ,return "Microcar"),(11  ,return "Lamborghini"),
                      (2919,return "VW")      ,(3   ,return "Morgan")  ,(1357,return "Nissan"),
                      (2049,return "Opel")    ,(3492,return "Peugeot") ,(2   ,return "Pontiac"),
                      (637 ,return "Porsche") ,(4874,return "Renault") ,(3   ,return "Rolls Royce"),
                      (14  ,return "Rover")   ,(24  ,return "Saab")    ,(1842,return "SEAT"),
                      (466 ,return "Skoda")   ,(691 ,return "Smart")   ,(24  ,return "Subaru"),
                      (83 ,return "Suzuki")   ,(47  ,return "Tesla")   ,(1130,return "Toyota"),
                      (5 ,return "Triumph")   ,(1316,return "Volvo")   ,(390 ,return "Mitsubishi")]

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

genEmail :: Gen Email
genEmail = do l1 <- choose (0::Int, 999999999)
              l2 <- return "@gmail.com"
              return ((show l1) ++ l2)

genNome :: Gen Nome
genNome = frequency [(4809,return "Maria")    ,(2062,return "Matilde")  ,(1859,return "Leonor"),
                     (1378,return "Beatriz")  ,(1330,return "Mariana")  ,(1295,return "Carolina"),
                     (1120,return "Ana")      ,(1062,return "Ines")     ,(980 ,return "Sofia"),
                     (930 ,return "Margarida"),(929 ,return "Lara")     ,(708 ,return "Joana"),
                     (651 ,return "Laura")    ,(650 ,return "Francisca"),(573 ,return "Diana"),
                     (517 ,return "Mafalda")  ,(511 ,return "Madalena") ,(504 ,return "Clara"),
                     (500 ,return "Luana")    ,(488 ,return "Sara")     ,(482 ,return "Bianca"),
                     (461 ,return "Alice")    ,(444 ,return "Rita")     ,(350 ,return "Eva"),
                     (340 ,return "Gabriela") ,(326 ,return "Camila")   ,(315 ,return "Yara"),
                     (313 ,return "Benedita") ,(312 ,return "Mara")     ,(295 ,return "Catarina"),
                     (278 ,return "Ariana")   ,(244 ,return "Ema")      ,(1809,return "Joao"),
                     (243 ,return "Vitoria")  ,(242 ,return "Marta")    ,(230 ,return "Carlota"),
                     (1783,return "Rodrigo")  ,(1718,return "Francisco"),(1663,return "Martim"),
                     (1428,return "Santiago") ,(1400,return "Tomas")    ,(1378,return "Afonso"),
                     (1244,return "Duarte")   ,(1207,return "Miguel")   ,(1206,return "Guilherme"),
                     (1140,return "Tiago")    ,(1133,return "Goncalo")  ,(1129,return "Diogo"),
                     (1128,return "Gabriel")  ,(1061,return "Pedro")    ,(993 ,return "Rafael"),
                     (993 ,return "Salvador") ,(810 ,return "Dinis")    ,(772 ,return "Lucas"),
                     (720 ,return "Simao")    ,(706 ,return "Gustavo")  ,(689 ,return "David"),
                     (683 ,return "Jose")     ,(648 ,return "Vicente")  ,(622 ,return "Lourenço"),
                     (595 ,return "Diego")    ,(542 ,return "Daniel")   ,(519 ,return "Antonio"),
                     (492 ,return "Andre")    ,(490 ,return "Vasco")    ,(455 ,return "Manuel"),
                     (433 ,return "Henrique") ,(383 ,return "Leonardo") ,(378 ,return "Bernardo")]

genMorada :: Gen Morada
genMorada = do a <- elements listaLocalidades
               return a

genPreferencia :: Gen Preferencia
genPreferencia = do a <- elements listaPreferencias 
                    return a

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

genAluguer :: [NifCliente] -> Gen Aluguer 
genAluguer lista = do a <- elements lista 
                      b <- genX
                      c <- genY
                      d <- genTipo
                      e <- genPreferencia
                      return (Aluguer a b c d e)

genClassificar :: [ClassProp] -> Gen Classificar
genClassificar lista = do a <- elements lista
                          b <- elements [0..300]
                          return (Classificar a b)

-- Funções auxiliares
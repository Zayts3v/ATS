import Test.QuickCheck

import Listas
import Data.List
import Control.Monad
import Control.Monad.State

{--
    Carro:        tipo,marca,matricula,nif,velocidade media,preço por km, consumo por km, autonomia, X, Y
    Proprietario: nome,nif,email,morada
    Cliente:      nome,nif,email,morada,X,Y
    Aluguer:      nif cliente, X destino, Y destino, tipoCombustivel , preferencia
    Classificar:  matricula ou nif (cliente ou prop) , nota (0-100)
-}

data Tipo = Gasolina
          | Electrico
          | Hibrido
            deriving Show

type Matricula   = String
type NIF         = Int
type VelMedia    = Int
type PPKm        = Float
type CPKm        = Float
type Autonomia   = Int
type X           = Float
type Y           = Float
type Nota        = Int


-- Main
main :: IO ()
main = do putStrLn "Give me a file to print the output"
          file    <- getLine
          putStrLn "Give a number to generate"
          inputjar <- getLine
          let lognumber = read inputjar :: Int
          initial <- generate $ genInitial
          writeFile file $ unlines $ filter (/="") initial     
          content <- readFile file
          let strings = lines content
          let nifC    = (drop 2 (take 3 strings))
          let nifP    = (drop 3 strings)
          output  <- generate $ runGen (genLogs lognumber) nifC nifP
          putStrLn "Give me a file to print the output"
          file2   <- getLine
          let cliente = (take 1 strings)
          let prop    = (take 1 (drop 1 strings))
          let output2 = (cliente ++ prop)
          writeFile file2 $ unlines $ filter (/="") (output2 ++ reverse(output))

-- State
genLogs lognumber = fmap sort . replicateM lognumber $ do
    g <- lift $ frequency [(8,return genProprietario),(10,return genCliente),(60,return genCarro),
                           (14,return genAluguer),(8,return genClassificar)] :: StGen (StGen String)
    g

type StGen a = StateT GenState Gen a

data GenState
    = GenState
    { nifClientes :: [String]
      , nifProp   :: [String]
      , classprop :: [String]
    } deriving Show

defaultState :: GenState
defaultState
    = GenState
    { nifClientes = []
      , nifProp   = []
      , classprop = []
    }

runGen :: StGen a -> [String] -> [String] -> Gen a
runGen g nifC nifP = evalStateT g defaultState { nifClientes = nifC , nifProp = nifP }

-- Gens
genInitial :: Gen [String]
genInitial = do nomeC   <- elements listaNomes
                nifC    <- genNif []
                emailC  <- return (show(nifC) ++ "@gmail.com")
                moradaC <- elements listaLocalidades
                xC      <- genX
                yC      <- genY
                nomeP   <- elements listaNomes
                nifP    <- genNif [show(nifC)]
                emailP  <- return (show(nifP)++"@gmail.com")
                moradaP <- elements listaLocalidades
                let cliente      = ("NovoCliente:" ++ nomeC ++ "," ++ show(nifC) ++ "," ++ emailC ++ "," ++ moradaC ++ "," ++ show(xC) ++ "," ++ show(yC))
                let proprietario = ("NovoProp:" ++ nomeP ++ "," ++ show(nifP) ++ "," ++ emailP ++ "," ++ moradaP)
                return ([cliente]++[proprietario]++[show(nifC)]++[show(nifP)])

genTipo :: Gen Tipo
genTipo = frequency [(42091,return Gasolina),(1217,return Hibrido),(472,return Electrico)]

genMarca :: Gen String
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

genNif :: [String] -> Gen NIF
genNif lista = do nif <- choose (000000000::Int,999999999)
                  if (elem (show(nif)) lista)
                  then genNif lista
                  else return nif

genVelMedia :: Gen VelMedia
genVelMedia = choose (10::Int, 300)

genPPKm :: Gen PPKm
genPPKm = choose (1.0,3)

genCPKm :: Gen CPKm
genCPKm = choose (0.1,3)

genAutonomia :: Gen Autonomia
genAutonomia = choose (100::Int, 1000)

genX :: Gen X
genX = choose (-100.0,100)

genY :: Gen Y
genY = choose (-100.0,100)

genNota :: Gen Nota
genNota = elements [0..100]

-- StateGens
genCarro :: StGen String
genCarro =  do  tipo      <- lift $ genTipo
                marca     <- lift $ genMarca
                matricula <- lift $ genMatricula
                modify (\ state -> state { classprop = matricula:classprop state})
                lista     <- gets nifProp
                nif       <- lift $ elements lista
                velMedia  <- lift $ genVelMedia
                ppkm      <- lift $ genPPKm
                cpkm      <- lift $ genCPKm
                autonomia <- lift $ genAutonomia
                x         <- lift $ genX
                y         <- lift $ genY
                return ("NovoCarro:" ++ show(tipo) ++ "," ++ marca ++ "," ++ 
                         matricula ++ "," ++ nif ++ "," ++ show(velMedia) ++
                          "," ++ show(ppkm) ++ "," ++ show(cpkm) ++ "," ++
                           show(autonomia) ++ "," ++ show(x) ++ "," ++ show(y))

genCliente :: StGen String
genCliente = do nome   <- lift $ elements listaNomes
                lista  <- gets nifClientes
                nif    <- lift $ genNif lista
                modify (\ state -> state { nifClientes = show(nif):nifClientes state})
                modify (\ state -> state { classprop = show(nif):classprop state})
                email  <- lift $ return (show(nif)++"@gmail.com")
                morada <- lift $ elements listaLocalidades
                x      <- lift $ genX
                y      <- lift $ genY
                return $ ("NovoCliente:" ++ nome ++ "," ++ show(nif) ++ "," ++ email ++
                         "," ++ morada ++ "," ++ show(x) ++ "," ++ show(y))

genProprietario :: StGen String
genProprietario = do nome   <- lift $ elements listaNomes
                     lista  <- gets nifProp
                     nif    <- lift $ genNif lista
                     modify (\ state -> state { nifProp = show(nif):nifProp state})
                     modify (\ state -> state { classprop = show(nif):classprop state})
                     email  <- lift $ return (show(nif)++"@gmail.com")
                     morada <- lift $ elements listaLocalidades
                     return $ ("NovoProp:" ++ nome ++ "," ++ show(nif) ++
                                "," ++ email ++ "," ++ morada)

genAluguer :: StGen String
genAluguer = do nifC        <- gets nifClientes
                nif         <- lift $ elements nifC
                x           <- lift $ genX
                y           <- lift $ genY
                tipo        <- lift $ genTipo
                preferencia <- lift $ elements listaPreferencias
                return $ ("Aluguer:" ++ nif ++ "," ++ show(x) ++ "," ++ show(y) ++ "," ++
                           show(tipo) ++ "," ++ preferencia)

genClassificar :: StGen String
genClassificar = do identificacao  <- gets classprop
                    id             <- lift $ elements identificacao
                    nota           <- lift $ genNota
                    return $ ("Classificar:" ++ id ++ "," ++ show(nota))

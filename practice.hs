import Data.Time.Clock
import Data.List
import System.IO
import System.Directory (renameFile, removeFile)
import Control.Exception (try, SomeException)

-- Definición del tipo de datos para representar la información de un vehículo
data Vehiculo = Vehiculo {
    placa :: String,
    entrada :: UTCTime,
    salida :: Maybe UTCTime
} deriving (Show, Read)

-- Función para registrar la entrada de un vehículo al parqueadero
registrarEntrada :: String -> UTCTime -> [Vehiculo] -> [Vehiculo]
registrarEntrada p t = (Vehiculo p t Nothing :)

-- Función para registrar la salida de un vehículo del parqueadero
registrarSalida :: String -> UTCTime -> [Vehiculo] -> [Vehiculo]
registrarSalida p t = map (\v -> if p == placa v then v { salida = Just t } else v)

-- Función para buscar un vehículo por su placa en el parqueadero
buscarVehiculo :: String -> [Vehiculo] -> Maybe Vehiculo
buscarVehiculo p = find (\v -> p == placa v && isNothing (salida v))
  where
    isNothing Nothing = True
    isNothing _       = False

-- Función para calcular el tiempo que un vehículo permaneció en el parqueadero
tiempoEnParqueadero :: Vehiculo -> UTCTime -> NominalDiffTime
tiempoEnParqueadero v = diffUTCTime (entrada v)

-- Función para guardar la información de los vehículos en un archivo de texto
guardarParqueadero :: [Vehiculo] -> IO ()
guardarParqueadero ps = do
    let tempFile = "parqueadero_temp.txt"
    res <- try (writeFile tempFile (unlines (map mostrarVehiculo ps))) :: IO (Either SomeException ())
    case res of
        Right _ -> do
            renameFile tempFile "parqueadero.txt"
            putStrLn "Parqueadero guardado en el archivo parqueadero.txt."
        Left ex -> do
            putStrLn $ "Error guardando el parqueadero: " ++ show ex
            removeFile tempFile

-- Función para cargar la información de los vehículos desde un archivo de texto
cargarParqueadero :: IO [Vehiculo]
cargarParqueadero = do
    content <- readFile "parqueadero.txt"
    return (map read (lines content))

-- Función para mostrar la información de un vehículo como cadena de texto
mostrarVehiculo :: Vehiculo -> String
mostrarVehiculo v = intercalate "," [placa v, show (entrada v), show (salida v)]

-- Función para listar los vehículos que están en el parqueadero
listarVehiculos :: [Vehiculo] -> IO ()
listarVehiculos ps = do
    putStrLn "Mostrando Lista de carros dentro del parqueadero"
    mapM_ (putStrLn . mostrarVehiculo) (filter (isNothing . salida) ps)
  where
    isNothing Nothing = True
    isNothing _       = False

-- Función principal del programa
main :: IO ()
main = do
    -- Cargar el parqueadero desde el archivo de texto
    parqueadero <- cargarParqueadero
    putStrLn "¡Bienvenido al Sistema de Gestión de Parqueadero!"
    -- Ciclo principal del programa
    cicloPrincipal parqueadero

-- Función para el ciclo principal del programa
cicloPrincipal :: [Vehiculo] -> IO ()
cicloPrincipal ps = do
    putStrLn "\nSeleccione una opción:"
    putStrLn "1. Registrar entrada de vehículo"
    putStrLn "2. Registrar salida de vehículo"
    putStrLn "3. Buscar vehículo por placa"
    putStrLn "4. Listar vehículos en el parqueadero"
    putStrLn "5. Salir"
    opcion <- getLine
    case opcion of
        "1" -> do
            putStrLn "Ingrese la placa del vehículo:"
            p <- getLine
            t <- getCurrentTime
            let psAct = registrarEntrada p t ps
            putStrLn $ "Vehículo con placa " ++ p ++ " ingresado al parqueadero."
            guardarParqueadero psAct
            cicloPrincipal psAct
        "2" -> do
            putStrLn "Ingrese la placa del vehículo a salir:"
            p <- getLine
            t <- getCurrentTime
            let psAct = registrarSalida p t ps
            putStrLn $ "Vehículo con placa " ++ p ++ " salido del parqueadero."
            guardarParqueadero psAct
            cicloPrincipal psAct
        "3" -> do
            putStrLn "Ingrese la placa del vehículo a buscar:"
            p <- getLine
            t <- getCurrentTime
            case buscarVehiculo p ps of
                Just v -> do
                    let tiempoTotal = tiempoEnParqueadero v t
                    putStrLn $ "El vehículo con placa " ++ p ++ " se encuentra en el parqueadero."
                    putStrLn $ "Tiempo en parqueadero: " ++ show tiempoTotal ++ " segundos."
                Nothing -> putStrLn "Vehículo no encontrado en el parqueadero."
            cicloPrincipal ps
        "4" -> do
            listarVehiculos ps
            cicloPrincipal ps
        "5" -> putStrLn "¡Hasta luego!"
        _ -> do
            putStrLn "Opción no válida. Por favor, seleccione una opción válida."
            cicloPrincipal ps

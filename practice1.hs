import Data.Time.Clock
import Data.List
import System.IO
import System.IO.Error (catchIOError)
import Data.Maybe (isNothing)
--librerias usadas

data Libro = Libro {
    titulo :: String,
    prestamo :: UTCTime,
    devolucion :: Maybe UTCTime
    } deriving (Show, Read)

registrarPrestamo :: String -> UTCTime -> [Libro] -> [Libro]
registrarPrestamo tituloLibro fechaPrestamo biblioteca =
    Libro tituloLibro fechaPrestamo Nothing : biblioteca


registrarDevolucion :: String -> UTCTime -> [Libro] -> [Libro]
registrarDevolucion tituloLibro fechaDevolucion biblioteca =
    map(\l -> if titulo l == tituloLibro
        then l { devolucion = Just fechaDevolucion }
        else l) biblioteca

buscarLibro :: String -> [Libro] -> Maybe Libro
buscarLibro tituloLibro biblioteca =
    find (\l -> tituloLibro == titulo l && isNothing(devolucion l)) biblioteca
    where isNothing Nothing = True
          isNothing _ = False  

--para calcular tiempo

calcularTiempoprestad :: Libro -> UTCTime -> NominalDiffTime
calcularTiempoprestad libro fechaActual =
    case devolucion libro of
        Just fechaDevolucion -> diffUTCTime fechaDevolucion (prestamo libro)
        Nothing -> diffUTCTime fechaActual (prestamo libro)


--guardarInfo
guardarInfo :: [Libro] -> IO ()
guardarInfo biblioteca = do
    writeFile "biblioteca.txt" (unlines (map mostrarLibro biblioteca))
    putStrLn "Informacion guardada en biblioteca.txt"

cargarInfo :: IO [Libro]--error sulucionado con ayuda de la IA
cargarInfo = catchIOError (do
    contenido <- readFile "biblioteca.txt"
    if null contenido
        then return []
        else do
            let libros = lines contenido
            return (map read libros)
    ) (\_ -> return [])


mostrarLibro :: Libro -> String
mostrarLibro libro =
    titulo libro ++ " | " ++ show (prestamo libro) ++ " | " ++ show (devolucion libro)

--main del programa
main :: IO ()
main = do
    biblioteca <- cargarInfo
    putStrLn "\nPrestamo biblioteca EAFIT."
    cicloPrincipal biblioteca




--ciclo principal


cicloPrincipal :: [Libro] -> IO ()
cicloPrincipal biblioteca = do
    putStrLn "\nSeleccione una opcion:"
    putStrLn "1| Registrar prestamo"
    putStrLn "2| Registrar devolucion"
    putStrLn "3| Buscar libro"
    putStrLn "4| Calcular tiempo prestado"
    putStrLn "5| Guardar informacion"
    putStrLn "6| Salir"
    opcion <- getLine
    case opcion of
        "1" -> do
            putStrLn "\nIngrese el titulo del libro:"
            tituloLibro <- getLine
            fechaPrestamo <- getCurrentTime
            let bibliotecaActualizada = registrarPrestamo tituloLibro fechaPrestamo biblioteca
            putStrLn $ "\nPrestamo registrado para el libro: " ++ tituloLibro
            cicloPrincipal bibliotecaActualizada
        "2" -> do
            putStrLn "\nIngrese el titulo del libro:"
            tituloLibro <- getLine
            fechaDevolucion <- getCurrentTime
            let bibliotecaActualizada = registrarDevolucion tituloLibro fechaDevolucion biblioteca
            putStrLn $ "\nDevolucion registrada para el libro: " ++ tituloLibro
            cicloPrincipal bibliotecaActualizada
        "3" -> do
            putStrLn "\nIngrese el titulo del libro:"
            tituloLibro <- getLine
            case buscarLibro tituloLibro biblioteca of
                Just libro -> putStrLn $ "\nLibro encontrado: " ++ mostrarLibro libro
                Nothing -> putStrLn "\nLibro no encontrado o ya devuelto."
            cicloPrincipal biblioteca
        "4" -> do
            putStrLn "\nIngrese el titulo del libro:"
            tituloLibro <- getLine
            fechaActual <- getCurrentTime
            case buscarLibro tituloLibro biblioteca of
                Just libro -> do
                    let tiempoPrestado = calcularTiempoprestad libro fechaActual
                    putStrLn $ "\nEl libro ha sido prestado por: " ++ show tiempoPrestado ++ " segundos."
                Nothing -> putStrLn "\nLibro no encontrado o ya devuelto."
            cicloPrincipal biblioteca

        "5" -> do
            guardarInfo biblioteca
            putStrLn "\nInformacion guardada. Saliendo..."
            cicloPrincipal biblioteca

        "6" -> putStrLn "\nFin del progama."
        _ -> do
            putStrLn "Opcion invalida."
            cicloPrincipal biblioteca


    

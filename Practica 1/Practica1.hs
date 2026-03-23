import Data.List(find,findIndex) 
import Data.Time 
import System.IO (readFile')
import Data.Char (isDigit)
main::IO()
main=do
    estudiantes <- cargar 
    bucle estudiantes 
    
    


bucle :: [String] -> IO()
bucle estudiantes = do 

    num <- menu
    if num == 6 then do
        putStrLn "Programa Finalizado"
    else do 
        actualizar <- ejecutar num estudiantes 
        guardar actualizar 
        bucle actualizar 
        
    
cargar :: IO[String]
cargar = do
    appendFile  "University.txt" ""                     --crea el archivo si no existe--
    listado <- readFile' "University.txt"                --guarda en listado un string con todo lo que encuentre en University.txt--
    let lista = lines listado                           --guarda en lista una lista con todo lo que habia en el archivo separando cada elemento con el salto de linea--
    return lista 





guardar :: [String] -> IO()
guardar lista = do
    writeFile "University.txt" (unlines lista )


menu :: IO Int
menu = do
    putStrLn "------Menu-----"
    putStrLn "1. Registro"
    putStrLn "2. Buscar Estudiante"
    putStrLn "3. Calcular Tiempo En La Universidad"
    putStrLn "4. Listado de estudiantes"
    putStrLn "5. Registrar Salida"
    putStrLn "6. Salir"
    opcion <- getLine
    if all isDigit opcion then do
        let numero = read opcion :: Int
        if 0 < numero && numero < 7 then return numero
        else do 
            putStrLn " Opcion no valida" 
            menu 
    else do
        putStrLn " Opcion no valida" 
        menu  
    



ejecutar :: Int -> [String] -> IO[String]
ejecutar numero lista 
    | numero == 1 = registro lista 
    | numero == 2 = buscar lista 
    | numero == 3 = calculoTiempo lista 
    | numero == 4 = listaEstudiantes lista     
    | numero == 5 = registrarSalida lista 

    |otherwise = do
         putStrLn "Opcion no valida"
         return lista 



registro :: [String] -> IO [String ]
registro lista = do 
    putStrLn"Ingrese el nombre del Estudiante"
    nombre <- getLine 

    putStrLn"Ingrese Id del estudiante:"
    ide <- getLine
    let estudiante = find (\x -> takeWhile (/= ',') x==ide)lista 
    case estudiante of 
        Just encontrado -> do
            putStrLn "Ya hay un estudiante con ese id dentro de la universidad"
            return lista 
        Nothing -> do 
            if ide =="" || nombre =="" then do
                putStrLn "Nombre o Id invalidos"
                return lista 
            else do
                tiempo <- getZonedTime                                           --Extrae la hora, el dia, y la zona horaria--
                let local = zonedTimeToLocalTime tiempo                           --Elimina la zona horaria--
                    hora = localTimeOfDay local                                   --Deja solo la horaen horas, minutos y segundos--
                    entrada = dosDigitos (todHour hora) ++ ":" ++ dosDigitos (todMin hora)            --Deja solo la hora--

                    datos = ide ++ "," ++ nombre ++ "," ++ entrada ++ "," ++ "00:00"
                putStrLn "Datos guardados con exito"
                return (datos:lista)



convertir :: String -> Int                                                    --pasa de HH:MM a minutos--
convertir hora =  
    let h = read (takeWhile (/= ':')hora) :: Int                            -- Guarda en h el numero de horas--
        m = read (tail(dropWhile (/= ':') hora)) :: Int                        --Guarda en m el numero de minutos--
        minutos = (h * 60) + m
    in minutos
        
formato :: Int -> String                                                    --paa de minutos a HH:MM--
formato minutos =                                        
    let h = minutos `div` 60       
        m = minutos `mod`60 
        hor = if h< 10 then "0" ++ show h
        else show h
        min = if m < 10 then "0" ++ show m 
        else show m 
    in  hor ++ ":" ++ min 


buscar :: [String] -> IO[String]
buscar lista = do
    putStrLn " Ingrese el Id del estudiante a buscar"
    ide <- getLine
    let resultado = find (\x -> takeWhile (/= ',')x==ide)lista 
    case resultado of
        Just estudiante  ->  do
            let tramo1 = tail(dropWhile(/= ',')estudiante)                  --tramo1= extrae el nombre, la hora de entrada del estudiante y hora de slaida--
                tramo2 = tail (dropWhile (/= ',')tramo1 )                        -- Extrae la hora en minutos a la que entro el estudiante y la hora de salida--
                     
                nombre = takeWhile(/= ',') tramo1
            putStrLn ("Nombre: " ++ nombre)
            
            let entrada = takeWhile (/= ',') tramo2
                salida = tail(dropWhile(/= ',')tramo2)
            putStrLn("Hora de entrada: " ++ entrada)

            if salida == "00:00" then do 
                putStrLn "El estudiante aun esta en la universidad"
                return lista 
            else do
                putStrLn ("Hora de salida: " ++ salida)
                return lista
        Nothing -> do 
            putStrLn("En este momento no hay ningun estudiante con ese id dentro de la universidad")
            return lista 



calculoTiempo ::  [String] -> IO [String]
calculoTiempo lista = do
    putStrLn "Ingrese el Id del estudiante que quiere saber cuanto tiempo estuvo en la universidad"
    ide <- getLine 
    let resultado = find (\x ->takeWhile( /= ',')x == ide) lista
    case resultado of 
        Just estudiante -> do
            let tramo1 = tail(dropWhile(/= ',')estudiante)                                       -- Extrae el nombre el nombre, hora de entrada y hora de salida en minutos del estudiante-- 
                tramo2 = tail (dropWhile (/= ',')tramo1)                                         --Extrae la hora de entrada y hora de salida en minutos del estudiante--
            if tail (dropWhile (/= ',') tramo2) /= "00:00" then do 
                let nombre = takeWhile (/= ',')tramo1
                putStrLn ("Nombre del estudiante : " ++ nombre) 

                let e = takeWhile(/= ',')tramo2                                            --Extrae la hora de entrada --
                    s = tail (dropWhile (/= ',')tramo2)                                         --Extrae la hora de salida --

                let    t = (convertir s) - (convertir e) 
                       tiempo = formato t 
                
                putStrLn ("El tiempo que estuvo el estudiante en la universidad fue (HH:MM) : " ++ tiempo)
                return lista 
            else do 
                putStrLn "El estudiante actualmente no ha salido de la universidad"
                return lista 
        Nothing -> do 
            putStrLn "Actualmente no se ha registrado entrada de ningun estudiante con ese Id"
            return lista 


datos:: String-> IO()
datos estudiante = do 
    let tramo1= tail (dropWhile (/= ',') estudiante)                            --Extrae el nombre, la hora de entrada y la hora de salida en minutos--
        tramo2=  tail (dropWhile (/= ',')tramo1)                                  --Extrae la hora de entrada y la hora de salida en minutos--
       
        ide = takeWhile (/= ',')estudiante
        nombre = takeWhile (/= ',')tramo1
        entrada = takeWhile ( /= ',')tramo2                                                  --Extrae hora de entrada--
        salida = tail (dropWhile (/= ',')tramo2)                                             --Extrae hora de salidas--
        
        
    putStrLn ("Nombre:" ++ nombre) 
    putStrLn ("Id: " ++ ide)
    putStrLn ("Hora de entrada: " ++ entrada)
    if salida /= "00:00" then do 
        putStrLn ("Hora de salida: " ++ salida)
        putStrLn "-----------------------------------------------------"
    else do 
         putStrLn "El estudiante esta actualmente en la universidad"
         putStrLn "-------------------------------------------------------"
    


listaEstudiantes :: [String] -> IO[String]
listaEstudiantes []= do
    putStrLn  "Actualmente no hay estudiantes registrados"
    return [] 

listaEstudiantes lista = do
    mapM_ datos lista 
    return lista



actualizar:: String -> String -> String -> String
actualizar ide salida estudiante = 
    let prueba =takeWhile (/= ',') estudiante 
        tramo1 = tail ( dropWhile (/= ',')estudiante)                                --Extrae nombre,hora de entrada y hora de salida del estudiante--
        tramo2 = tail (dropWhile (/= ',')tramo1)                                 --Extrae hora de entrada y hora de salida del estudiante--
        nombre = takeWhile (/= ',')tramo1 
        entrada = takeWhile (/= ',')tramo2 

    in if prueba==ide then ide ++ "," ++ nombre ++ "," ++ entrada ++ ","++ salida   
    else estudiante 


registrarSalida :: [String] -> IO[String]
registrarSalida lista = do 
    putStrLn "Ingrese el id del estudiante que va a salir:"
    ide <- getLine
    let resultado = find( \x -> takeWhile (/= ',')x == ide)lista
    case resultado of
        Just estudiante -> do
            let tramo1= tail(dropWhile (/= ',')estudiante)                        --Extrae el nombre, hora de entrada y hora de salida del estudiante--
                tramo2= tail(dropWhile (/= ',')tramo1)                                 --Extrae hora de entrada y hora de salida del estudiante--

                nombre = takeWhile (/= ',')tramo1
                entrada = takeWhile (/= ',')tramo2 
                s = tail (dropWhile (/= ',')tramo2)
                
            if s /= "00:00" then do 
                putStrLn "El estudiante ya tiene una salida registrada"
                return lista 
            else do 
                tiempo <- getZonedTime                              --Extrae la hora, el dia, y la zona horaria--
                let local = zonedTimeToLocalTime tiempo              --elimina la zona horaria--
                    hora = localTimeOfDay local                       --deja solo la hora en horas,minutos y segundos--
                    salida = dosDigitos (todHour hora) ++ ":" ++ dosDigitos (todMin hora)         --deja la hora de salida en HH:MM--

                    nuevaLista = map (actualizar ide salida) lista 
                putStrLn "Salida registrada con exito"
                return nuevaLista

        Nothing -> do
            putStrLn "Actualmente no hay ningunestudiante registrado con ese Id"
            return lista 


dosDigitos :: Int -> String 
dosDigitos num =
    if num<10 then "0" ++ show num
    else show num    
   
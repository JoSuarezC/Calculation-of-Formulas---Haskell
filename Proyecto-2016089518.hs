
-- Ejemplo de un ciclo interactivo en Haskell

-- El ciclo principal recibe un estado [(String,String)],
-- en cada iteración del ciclo principal, se lee una línea,
-- la cual se interpreta como si fuera un comando.
--    si empieza con "def"
--       generar un nuevo estado agregando al estado
--       anterior un nuevo par formado por la segunda y
--       tercera palabra del comando:
--       >> def a saludo
--    si empieza con "borrar"
--       eliminar del estado el primer par cuyo primer
--       componente sea igual a la segunda palabra del comando
--       >> borrar a
--    si empieza con "imp"
--       imprimir estado actual (print)
--       >> imp
--    si empieza con "fin"
--       terminar el ciclo
--       >> fin
--
-- Luego de interpretar el comando, se invoca recursivamente 
-- el ciclo principal con el nuevo estado si es del caso.

import Data.List (nub)

type Operacion = Int -> Int -> Int 
data Termino = Variable String | Entero Int
data Arbol = Hoja Termino | Nodo Operacion Arbol Arbol 
type Estado = [(String, [String], Arbol, [String], Arbol)]

instance Show (a-> b) where show f = ""

-- Estado = [ variable, [variables incognitas], arbol, [lista arbol vigente total], arbol vigente total]

main :: IO ()
main = do 
       mainloop [] -- Ejecutar mainloop con estado inicial nulo

-- mainloop es una función que recibe un Estado y  
-- retorna una acción la cual al ser ejecutada no retorna nada, 
-- pero puede tener efectos colaterales (lee, modifica, escribe)
mainloop :: Estado -> IO ()
mainloop estado = do
    -- putStr::String -> IO (), es una función que recibe una tira y 
    -- devuelve una acción, la cual al ser ejecutada imprime la tira 
    -- (efecto colateral) y retorna nada
    putStr ">> "
    
    -- getLine es una función que retorna una acción que al ser ejecutada
    -- retorna una tira; la construcción "<-" ejecuta la acción de getLine
    -- y extrae la tira leída    
    inpStr <- getLine
    
    -- procesar es una función "pura" que toma la tira de entrada y el estado,
    -- y devuelve una tripleta:
    --     Bool terminar que indica si se debe terminar el ciclo principal (fin)
    --     Estado nuevoestado obtenido al ejecutar el comando a partir del estado actual
    --     String salida con un texto que se imprime como resultado del comando    
    let (terminar,nuevoestado,salida) = procesar inpStr estado
    
    -- impresión de la salida provocada por la ejecución del comando
    putStrLn salida
    
    -- terminar el ciclo si el comando así lo indica
    -- en caso contrario usar recursión de cola para obtener 
    -- el siguiente comando con el nuevo estado
    if terminar
       then return ()  -- return crea una acción que al ser ejecutada no retorna nada
                       -- es lo que se supone que debe devolver mainloop
       else mainloop nuevoestado

-- procesar recibe un comando y un estado y calcula la tripleta que
-- se produce al ejecutar el comando y afectar al estado 
procesar :: String -> Estado -> (Bool, Estado, String)
procesar comando estado =
     -- tokens es la línea de comando separada en palabras
     -- dependiendo de la primera palabra se invoca a la
     -- función que implementa dicho comando
     case tokens!!0 of
          "ie" -> cmd_ie (tail tokens) estado
          "mv" -> cmd_mv (tail tokens) estado
          "ma" -> cmd_ma (tail tokens) estado
          "cv" -> cmd_cv (tail tokens) estado
          "cvo" -> cmd_cvo (tail tokens) estado
          "mp" -> cmd_mp (tail tokens) estado
          "et" -> cmd_et (tail tokens) estado
          -- comando fin: retornar tripleta que finaliza ciclo          
          "fin" -> (True, estado, "Saliendo...")
          _     -> cmd_desconocido (tokens!!0) comando estado
       where tokens = words comando

-- Función que maneja un comando desconocido
cmd_desconocido :: String -> String -> Estado -> (Bool, Estado, String)
cmd_desconocido cmd comando estado = (False, estado, mensaje)
       where mensaje = "Comando desconocido ("++ cmd ++"): '" ++ comando ++ "'"

cmd_borrar::[String] -> Estado -> (Bool, Estado, String)
cmd_borrar [] estado = (False, estado, "No se especificó qué borrar")


-- Bug: y = x + 3  ->  x = 4 * 50 
-- Ingresar ecuación -------------------------------------------------------------------------------------------------------------
cmd_ie ::  [String] -> Estado -> (Bool, Estado, String)
cmd_ie linea estado  -- linea = ["y","=","a","+","b"]  estado = [String, [String], Arbol, [String], Arbol]
    | (length(linea) > 5) || (linea!!1 /= "=") || ((linea!!3 `elem` ["+","*","-"]) == False ) || ((esEntero (linea!!0)) == True)= (False, estado, "Error! Las ecuaciones ingresadas deben seguir la estructura: Variable = Término Operación Término")
    | (ie_validacion1 (linea!!0) estado) == True = (False, estado, "Ocurrió un error! La variable "++ linea!!0++" se encuentra previamente definida.")
    | (ie_validacion2 (head linea) (tail linea)) == True = (False, estado, "Ocurrió un error! La variable "++ linea!!0++" es un término dentro de la ecuación dada.")
    | (ie_validacion3 (head linea) (tail linea) estado) == True = (False, estado, "Error! La ecuación ingresada genera un bucle")
    | otherwise = (False, (actualizarEstado (init estadoNuevo) [(last estadoNuevo)]), mensaje)
    where estadoNuevo = estado ++ [(linea!!0, (listaVar (crearArbol (drop 2 linea))), (crearArbol (drop 2 linea)), (listaVar(actualizarEcuacion estado (listaVar (crearArbol (drop 2 linea))) (crearArbol (drop 2 linea)))),  (actualizarEcuacion estado (listaVar (crearArbol (drop 2 linea))) (crearArbol (drop 2 linea))))]
          mensaje = imprimirEstado (last estadoNuevo)

-- Validación 1: La variable a la izquierda de la ecuación no debe tener una ecuación previa
ie_validacion1 :: String -> Estado -> Bool
ie_validacion1 var [] = False
ie_validacion1 var ((x, l1, a1, l2, a2):grupoEstados)
    | (var == x) = True
    | otherwise = ie_validacion1 var grupoEstados

-- Validación 2: La variable a la izquierda de la ecuación no debe aparecer a la derecha de esa misma ecuación
ie_validacion2 :: String -> [String] -> Bool
ie_validacion2 var [] = False
ie_validacion2 var (t1:ec)
    | (var == t1) = True
    | otherwise = ie_validacion2 var ec

-- Validación 3: La nueva ecuación no debe formar un ciclo de dependencias con las ecuaciones anteriores; esto es para cada una de las ecuaciones anteriores, revisar que la variable de la nueva ecuación no aparece a la derecha de una ecuación anterior y que la variable de esa ecuación anterior no aparece a la derecha de la nueva ecuación; para esta revisión debe usar las ecuaciones vigentes 
 
ie_validacion3 :: String -> [String] -> Estado -> Bool
ie_validacion3 varEcNueva ecuacion [] = False
ie_validacion3 varEcNueva ecuacion ((v, y, q, w, p):grupoEstados)
    | (varEcNueva `elem` w) && (v `elem` ecuacion) =  True
    | otherwise = ie_validacion3 varEcNueva ecuacion grupoEstados

--{ y, [a, b], a + b, [a, c], a + (2 + c)}
imprimirEstado :: (String, [String], Arbol, [String], Arbol) -> String
imprimirEstado (v, y, q, w, p) = "{" ++ v ++ "," ++ " "++ "[" ++ (mostrarListaVar y) ++ "," ++ " "++(mostrarArbol q) ++ "," ++ " "++ "[" ++ (mostrarListaVar w) ++ "," ++ " "++(mostrarArbol p) ++ "}" 

mostrarListaVar :: [String] -> String
mostrarListaVar [] = "]"
mostrarListaVar (x:[]) = (obtenerStringValor x) ++ "]"
mostrarListaVar (x:restoLista) = (obtenerStringValor x) ++ "," ++ " " ++ (mostrarListaVar restoLista)

obtenerStringValor :: String -> String
obtenerStringValor valor
    | (esEntero valor) = show(valor)
    | otherwise = valor

-- Al estado nuevo añadir las variables del grupo de estados
actualizarEcuacion :: Estado -> [String] -> Arbol -> Arbol
actualizarEcuacion [] listaEc arbolEc = arbolEc
actualizarEcuacion ((v, y, q, w, p):grupoEstados) listaEc arbolEc
    | ((v `elem` listaEc) == True) =  actualizarEcuacion grupoEstados (listaVar(sustVar v p arbolEc )) (sustVar v p arbolEc)
    | otherwise = actualizarEcuacion grupoEstados listaEc arbolEc

-- Al grupo de estados añadirle la variable del estado nuevo
actualizarEstado :: Estado -> Estado -> Estado
actualizarEstado [] estadoCreado = estadoCreado
actualizarEstado ((v, y, q, w, p):grupoEstados) estadoCreado
    | ((fstEstado(head estadoCreado) `elem` w) == True) = [(v, y, q, listaVar(sustVar (fstEstado(head estadoCreado)) (fithEstado(head estadoCreado)) p), (sustVar (fstEstado(head estadoCreado)) (fithEstado(head estadoCreado)) p) )] ++ (actualizarEstado grupoEstados estadoCreado)
    | otherwise = [(v, y, q, w, p)] ++ (actualizarEstado grupoEstados estadoCreado)


-- Sacar elementos de las tuplas
fstEstado :: (String, [String], Arbol, [String], Arbol) -> String
fstEstado (var, _, _, _, _) = var

sndEstado :: (String, [String], Arbol, [String], Arbol) -> [String]
sndEstado (_, lista, _, _, _) = lista

trdEstado :: (String, [String], Arbol, [String], Arbol) -> Arbol
trdEstado (_, _, arbol, _, _) = arbol

fthEstado :: (String, [String], Arbol, [String], Arbol) -> [String]
fthEstado (_, _, _, lista, _) = lista

fithEstado :: (String, [String], Arbol, [String], Arbol) -> Arbol
fithEstado (_, _, _, _, arbol) = arbol

-- Mostrar variable -------------------------------------------------------------------------------------------------------------
cmd_mv ::  [String] -> Estado -> (Bool, Estado, String)
cmd_mv linea estado 
    | (length(linea) /= 1) = (False, estado, "Debe ingresar la variable que desea que se muestre su información")
    | esEntero(linea!!0) == True = (False, estado, "Ocurrió un error! "++linea!!0++" es un número. Debe insertar solo variables!")
    | (verificarExistenciaVariable (linea!!0) estado) == False = (False, estado, "La variable "++ linea!!0++" no se encuentra definida en el sistema.")
    | otherwise = (False, estado, mensaje)
    where mensaje = mostrarVariable (linea!!0) estado

mostrarVariable :: String -> Estado -> String
mostrarVariable var [] = []
mostrarVariable var ((v, y, q, w, p):grupoEstados)
    | v == var = imprimirEstado (v, y, q, w, p)
    | otherwise = mostrarVariable var grupoEstados

verificarExistenciaVariable :: String -> Estado -> Bool
verificarExistenciaVariable var [] = False
verificarExistenciaVariable var ((v, y, q, w, p):grupoEstados)
    | v == var = True
    | otherwise = verificarExistenciaVariable var grupoEstados

-- Mostrar ambiente -------------------------------------------------------------------------------------------------------------
cmd_ma ::  [String] -> Estado -> (Bool, Estado, String)
cmd_ma linea [] = (False, [], "El ambiente se encuentra vacío.")
cmd_ma linea estado  
    | (linea /= []) = (False, estado, "Debe ingresar solamente el comando ma")
    | otherwise = (False, estado, mensaje)
    where mensaje = mostrarAmbiente estado

mostrarAmbiente :: Estado -> String
mostrarAmbiente [] = ""
mostrarAmbiente ((v, y, q, w, p):grupoEstados) = imprimirEstado (v, y, q, w, p) ++ "\n" ++ (mostrarAmbiente grupoEstados)

-- Calcular variable -------------------------------------------------------------------------------------------------------------
cmd_cv :: [String] -> Estado -> (Bool, Estado, String)
cmd_cv linea [] = (False, [], "Ocurrió un error! El ambiente se encuentra vacío.")
cmd_cv linea estado  
    |(linea == []) = (False, estado, "Debe ingresar los valores para los parámetros de la variable original.")
    | (esEntero (linea!!0)) == True = (False, estado, "Ingrese la variable a la que desea calcular su valor original en la primera posición del comando")
    | (verificarExistenciaVariable (linea!!0) estado) == False  = (False, estado, "La variable indicada no se encuentra declarada")
    | (parametrosNumericos (tail linea)) == False = (False, estado, "Los valores de los parámetros deben ser numéricos")
    | otherwise = (False, estado, mensaje)
    where mensaje = show (calcularValor (head linea) (tail linea) estado)

calcularValor :: String -> [String] -> Estado -> Int
calcularValor var lista ((v, y, q, w, p):grupoEstados)
    | (var == v) = (evalArb p (zip w (convertirLista lista)))
    | otherwise = (calcularValor var lista grupoEstados)

convertirLista :: [String] -> [Int]
convertirLista [] = []
convertirLista (x:xs) = (read x :: Int):[] ++ (convertirLista xs)

parametrosNumericos :: [String] -> Bool
parametrosNumericos [] = True
parametrosNumericos (x:xs)
    | (esEntero (x)) == True = parametrosNumericos xs
    | otherwise = False

-- Calcular variable original -----------------------------------------------------------------------------------------------------
cmd_cvo :: [String] -> Estado -> (Bool, Estado, String)
cmd_cvo linea [] = (False, [], "El ambiente se encuentra vacío.")
cmd_cvo linea estado  
    |(linea == []) = (False, estado, "Debe ingresar los valores para los parámetros de la variable original.")
    | (esEntero (linea!!0)) == True = (False, estado, "Ingrese la variable a la que desea calcular su valor original en la primera posición del comando")
    | (verificarExistenciaVariable (linea!!0) estado) == False  = (False, estado, "La variable indicada no se encuentra declarada")
    | (parametrosNumericos (tail linea)) == False = (False, estado, "Los valores de los parámetros deben ser numéricos")
    | otherwise = (False, estado, mensaje)
    where mensaje = show (calcularValorOriginal (head linea) (tail linea) estado)

calcularValorOriginal :: String -> [String] -> Estado -> Int
calcularValorOriginal var lista ((v, y, q, w, p):grupoEstados)
    | (var == v) = (evalArb p (zip y (convertirLista lista)))
    | otherwise = (calcularValorOriginal var lista grupoEstados)



-- Mostrar parámetros -------------------------------------------------------------------------------------------------------------
cmd_mp :: [String] -> Estado -> (Bool, Estado, String)
cmd_mp linea [] = (False, [], "El ambiente se encuentra vacío.")
cmd_mp linea estado  -- linea = ["y","=","a","+","b"]  estado = [String, [String], Arbol, [String], Arbol]
    | (linea /= []) = (False, estado, "Debe ingresar solamente el comando mp")
    | otherwise = (False, estado, mensaje)
    where mensaje = "[" ++ (mostrarListaVar (obtenerParametros estado))

obtenerParametros :: Estado -> [String]
obtenerParametros [] = []
obtenerParametros ((v, y, q, w, p):grupoEstados) = nub (w ++ (obtenerParametros grupoEstados))



-- Evaluar todo ----------------------------------------------------------------------------------------------------------------
cmd_et :: [String] -> Estado -> (Bool, Estado, String)
cmd_et linea [] = (False, [], "El ambiente se encuentra vacío.")
cmd_et linea estado  -- linea = ["y","=","a","+","b"]  estado = [String, [String], Arbol, [String], Arbol]
    |(linea == []) = (False, estado, "Ingrese los valores para los parámetros.")
    | (parametrosNumericos (tail linea)) == False = (False, estado, "Error! Los valores para los parámetros deben ser numéricos")
    | (length (obtenerParametros estado)) /= (length linea) = (False, estado, "Error! La cantidad de valores no concuerda con la cantidad de parámetros. Inténtelo de nuevo.")
    | otherwise = (False, estado, mensaje)
    where mensaje = mostrarListaTupla(calcularEcuaciones linea (obtenerParametros estado) estado)

calcularEcuaciones :: [String] -> [String] -> Estado -> [(String, String)] --[ListaValores], [ListaParametros], Estado, Resultado
calcularEcuaciones listaValores listaParametros [] = []
calcularEcuaciones listaValores listaParametros ((v, y, q, w, p):grupoEstados) = (zip (v:[]) ((show(evalArb p (asignarValores w listaParametros listaValores))):[])) ++ (calcularEcuaciones listaValores listaParametros grupoEstados)

asignarValores :: [String] -> [String] -> [String] -> [(String, Int)]
asignarValores [] listaParametros listaValores = []
asignarValores (x:xs) listaParametros listaValores = (generarTupla x listaParametros listaValores) ++ (asignarValores xs listaParametros listaValores)

generarTupla :: String -> [String] -> [String] -> [(String, Int)]
generarTupla variable [] [] = []
generarTupla variable (y:ys) (z:zs)
    | variable == y = zip (variable:[]) ((read z :: Int):[])
    | otherwise = generarTupla variable ys zs

mostrarListaTupla :: [(String, String)] -> String
mostrarListaTupla (x:[]) = "("++ (fst x) ++ ", " ++ (snd x) ++ ")]"
mostrarListaTupla (x:restoLista) = "("++ (fst x) ++ ", " ++ (snd x) ++ "), " ++ (mostrarListaTupla restoLista)


-- Recorrer estado por estado y revisar cuales parametros calzan con mp. Luego mandar v p y zip w (linea con terminos iguales)
-- **********************************Estructuras*********************************
  
-- ******************************************************************************



-- Recibe: x + y
crearArbol :: [String] -> Arbol
crearArbol ecuacion = if length(ecuacion) == 1 
                        then Hoja (crearHoja (head (ecuacion))) --["a"] head -> "a"
                        else Nodo (transformarOperacion (ecuacion !!1)) (crearArbol [head ecuacion]) (crearArbol [ecuacion !!2])

transformarOperacion :: String -> Operacion
transformarOperacion "+" = (+)
transformarOperacion "-" = (-)
transformarOperacion "*" = (*)

mostrarArbol :: Arbol -> String 
mostrarArbol (Hoja valor) = (obtenerHojaValor(valor))
mostrarArbol (Nodo x i d) = "(" ++ (mostrarArbol i) ++ " " ++ (transformarOperacionToString x) ++ " " ++ (mostrarArbol d) ++ ")"

transformarOperacionToString :: Operacion -> String
transformarOperacionToString op
  | ((op) 2 1 == 3) = "+"
  | ((op) 2 1 == 1) = "-"
  | ((op) 2 1 == 2) = "*"

crearHoja :: String -> Termino 
crearHoja var = if ((esEntero var) == True)
                then (Entero (read var :: Int))
                else (Variable var)
esEntero var = case reads var :: [(Integer, String)] of
  [(_,"")] -> True
  _ -> False




{- 
3.
Elabore una función sustVar que tome una variable y un Arbol que representa su ecuación y sustituya en otro Arbol las apariciones de esa variable por copias del Arbol asociado.:
sustVar :: Variable -> Arbol -> Arbol -> Arbol -> [String] 
sustVar b <Árbol para (2+c)> <Árbol para ((a+b)*z)>

produce <Árbol para ((a+(2+c))*z)> 


-}

sustVar :: String -> Arbol -> Arbol -> Arbol
sustVar variable arbolVariable (Nodo x i d) = (Nodo x (sustVar variable arbolVariable i) (sustVar variable arbolVariable d))
sustVar variable arbolVariable (Hoja h)
    | ((obtenerHojaValor h) == variable) = (arbolVariable)
    | otherwise = (Hoja h)


obtenerHojaValor :: Termino -> String
obtenerHojaValor (Variable valor) = valor
obtenerHojaValor (Entero valor) = show(valor)


{-
4.
Elabore una función listaVar que tome un Arbol y devuelva una lista con las variables que aparecen en dicho árbol; cada variable debe aparecer una sola vez en la lista:
listaVar :: Arbol -> [String]
listarVar (crearArbol "x + 3") produce ["x"]
listarVar <Árbol para (a+(2+c))*z> produce ["a","c","z"]
-}



listaVar ::  Arbol -> [String]
listaVar (Nodo x l r) = nub ((listaVar l) ++ (listaVar r))



listaVar (Hoja variable) 
    | ((esEntero(obtenerHojaValor variable)) == False) = ((obtenerHojaValor variable):[])
    | otherwise = []



{-
5.
Elabore una función evalArb que tome un Arbol y una lista de valores, y devuelva el resultado de evaluar dicho Arbol usando esos valores; los valores se asocian con las variables siguiendo el orden especificado por el resultado de listaVar para ese Arbol:
evalArb :: Arbol -> [Int] -> Int
evalArb <Árbol para (a+(2+c))*z> [3,1,5]

produce (3+(2+1))*5 = 30

porque el orden de los parámetros dado por listaVar es ["a","c","z"],

de modo que a=3, c=1, z=5.


Una alternativa es que la lista de valores ya venga asociada con los nombres:
evalArb <Árbol para (a+(2+c))*z> [("a",3),("c",1),( "z",5)]
-}


evalArb :: Arbol -> [(String, Int)] -> Int
evalArb (Nodo x l r) listaNum = ((x) (evalArb l listaNum) (evalArb r listaNum))   --Si la izq es nodo
evalArb (Hoja h) listaNum
    | ((esEntero(obtenerHojaValor h)) == False) = (sustitucionNumerica (obtenerHojaValor h) listaNum 0)
    | otherwise = read (obtenerHojaValor h) :: Int

-- Recorre la listaNum en busca del valor de la variable enviada.
sustitucionNumerica :: String -> [(String, Int)] -> Int -> Int
sustitucionNumerica variable listaNum indice
    | (variable == (fst(listaNum !! indice))) = snd(listaNum !! indice)
    | otherwise = (sustitucionNumerica variable listaNum (indice + 1))




-- ******************************************************************************
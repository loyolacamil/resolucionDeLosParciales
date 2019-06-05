module Deadpool where

import Text.Show.Functions
import Data.List

-----------------------------------------------------------------------------------
--
-- Importo Data.List porque tiene una funcion que se llama genericLength
-- 
-- genericLength :: Num b => [a] -> b

-- Como notarán es un length pero en lugar de devolver un número entero, 
-- devuelve un número más genérico (que puedo dividir)
--
-- En un parcial pueden usar length tranquilamente, pero sino en la compu no anda
--
-----------------------------------------------------------------------------------


--------------
-- Punto 01 --
--------------

data Mutante = Mutante {
  nombre      :: String,
  vida        :: Float,
  habilidades :: [String],
  armas       :: [Arma]
} deriving (Show)

type Arma = Mutante -> Mutante


--------------
-- Punto 01 --
--------------

estaMuerto :: Mutante -> Bool

estaMuerto = (== 0) . vida

estaMuertoVersion2 mutante = ((== 0) . vida) mutante

estaMuertoVersion3 mutante = (== 0) . vida $ mutante

estaMuertoVersion4 mutante = (vida mutante) == 0

estaMuertoVersion5 (Mutante _ 0 _ _ ) = True
estaMuertoVersion5 _                  = False

estaMuertoVersion6 (Mutante _ vida _ _ ) = vida == 0

-- Tener en cuenta que es preferible componer funciones antes de aplicar pattern matching


--------------
-- Punto 02 --
--------------

esFrancis :: Mutante -> Bool

esFrancis = seLlama "Francis"

esFrancisVersion2 mutante = seLlama "Francis" mutante

esFrancisVersion3 (Mutante "Francis" _ _ _ ) = True
esFrancisVersion3 _                          = False

esFrancisVersion4 (Mutante nombre _ _ _ ) = nombre == "Francis"

-- Tener en cuenta que es preferible componer funciones antes de aplicar pattern matching


seLlama unNombre = (== unNombre) . nombre

seLlamaVersion2 unNombre mutante = ((== unNombre) . nombre) mutante

seLlamaVersion3 unNombre mutante = (== unNombre) . nombre $ mutante

seLlamaVersion4 unNombre mutante = nombre mutante == unNombre


--------------
-- Punto 03 --
--------------

isMyDad :: Mutante -> Bool

isMyDad mutante = seLlama "Coloso" mutante || todasHabilidadesMetalicas mutante


todasHabilidadesMetalicas = all esMetalica . habilidades

todasHabilidadesMetalicasVersion2 mutante = (all esMetalica . habilidades) mutante

todasHabilidadesMetalicasVersion3 mutante = all esMetalica . habilidades $ mutante

todasHabilidadesMetalicasVersion4 mutante = all esMetalica (habilidades mutante)


esMetalica = elem "metal" . words

esMetalicaVersion2 habilidades = (elem "metal" . words) habilidades

esMetalicaVersion3 habilidades = elem "metal" . words $ habilidades

esMetalicaVersion4 habilidades = elem "metal" (words habilidades)


--------------
-- Punto 04 --
--------------

punio :: Mutante -> Mutante
--    :: Arma

punio enemigo
  | tieneHabilidad "Esquivar Golpes" enemigo = enemigo
  | otherwise                                = restarVida 5 enemigo


punioVersion2 enemigo = restarVida (cantidadDeVidaARestarAl enemigo) enemigo


cantidadDeVidaARestarAl enemigo
  | tieneHabilidad "Esquivar Golpes" enemigo = 5
  | otherwise                                = 0


tieneHabilidad habilidad = elem habilidad . habilidades

tieneHabilidadVersion2 habilidad mutante = (elem habilidad . habilidades) mutante

tieneHabilidadVersion3 habilidad mutante = elem habilidad . habilidades $ mutante

tieneHabilidadVersion4 habilidad mutante = elem habilidad (habilidades mutante)


restarVida cantidad mutante = mutante {
    vida = max 0 (vida mutante - cantidad)
}

restarVidaVersion2 cantidad (Mutante nombre vida habilidades armas) = (Mutante nombre (max 0 (vida - cantidad)) habilidades armas)


--------------
-- Punto 05 --
--------------

pistola :: Float -> Mutante -> Mutante
--      :: Float -> Arma

pistola calibre enemigo = restarVida (calibre * genericLength (habilidades enemigo)) enemigo

-- Recuerden:
--
--   genericLength :: Num b :: [a] -> b


--------------
-- Punto 06 --
--------------

espada :: Float -> Mutante -> Mutante
--     :: Float -> Arma

espada fuerza = restarVida (fuerza / 2)


--------------
-- Punto 07 --
--------------

wade = Mutante "Wade Wilson" 100 ["Estar enamorado", "Regeneracion"] [punio, pistola 9]

abastecernos _ (Mutante "Deadpool" vida habilidades _) = (Mutante "Deadpool" vida habilidades [pistola 9, espada 16, espada 16])
abastecernos masArmas mutante = mutante {
  armas = masArmas ++ armas mutante
}


--------------
-- Punto 08 --
--------------

ataqueRapido :: Mutante -> Mutante -> Mutante

ataqueRapido enemigo = ($ enemigo) . head . armas

ataqueRapidoVersion2 enemigo atacante = (($ enemigo) . head . armas) atacante

ataqueRapidoVersion3 enemigo atacante = (head (armas atacante)) enemigo


--------------
-- Punto 09 --
--------------

atacarConTodo :: Mutante -> Mutante -> Mutante

atacarConTodo enemigo = foldl (flip ($)) enemigo . armas
-- En un parcial el flip no me suma, pero en la compu lo tengo que poner para que ande.

atacarConTodoVersion2 enemigo atacante = (foldl (flip ($)) enemigo . armas) atacante

atacarConTodoVersion3 enemigo atacante = foldl (flip ($)) enemigo . armas $ atacante

atacarConTodoVersion4 enemigo = ($ enemigo) . foldr (.) id . armas

atacarConTodoVersion5 enemigo atacante = (foldr (.) id (armas atacante)) enemigo 

atacarConTodoVersion6 enemigo atacante = atacarRecursivoParaLaVersion6 enemigo (armas atacante)

atacarRecursivoParaLaVersion6 enemigo []           = enemigo
atacarRecursivoParaLaVersion6 enemigo (arma:armas) = atacarRecursivoParaLaVersion6 (arma enemigo) armas


--------------
-- Punto 10 --
--------------

todosAUno :: Mutante -> [Mutante] -> Mutante

todosAUno enemigo = foldl atacarConTodo enemigo

todosAUnoVersion2 enemigo atacantes = foldl atacarConTodo enemigo atacantes


--------------
-- Punto 11 --
--------------

comoEstaSuFamilia :: Mutante -> [Mutante] -> [String]

comoEstaSuFamilia atacante = map nombre . filter estaMuerto . atacarRapidamenteATodos atacante

atacarRapidamenteATodos atacante = map (flip ataqueRapido atacante)


--------------
-- Punto 12 --
--------------

rescatarASuChica :: Mutante -> [Mutante] -> [Mutante] -> [Mutante]

rescatarASuChica atacante amigos = map (flip todosAUno amigos) . filter (not . estaMuerto) . atacarRapidamenteATodos atacante


--------------
-- Punto 13 --
--------------

cualEsMiNombre :: (Mutante -> [a]) -> Float -> (Mutante -> b) -> [Mutante] -> [a]

cualEsMiNombre x y z = concatMap fst . map (\n -> (x n, z n)) . filter ((y>).vida)

--------------
-- Punto 14 --
--------------

-- a
chuck = Mutante "Chuck" 1000000 ["Patada Giratoria"] (repeat punio)

-- b 
-- Claro que sí, el enemigo le resta vida, por lo que no es necesario que evalúe la lista de armas completa

-- c
-- No, porque nunca termina de atacar al mutante enemigo, por lo que nunca termina de restarle vida al enemigo




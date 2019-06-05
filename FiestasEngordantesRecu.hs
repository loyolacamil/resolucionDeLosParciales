module Recuperatorio where 

import Data.List
import Text.Show.Functions

--punto 1--
listaDeC:: (c->Bool)->(c->c)->[c]->[c]
listaDeC a b c = filter (not . a) c ++ (map b . filter a) c

type CaloriasIngeridas = Float
type Nombre = String
data Nutriente = 
    VitaminaA | VitaminaC 
    | VitaminaE | VitaminaK 
    | Hierro | Calcio | Fibra
    deriving(Show)

data Persona = UnaPersona {
    nombre::Nombre,
    caloriasIngeridas::CaloriasIngeridas,
    nutrientesIngeridos::[Nutriente]
}deriving (Show)

sofia::Persona
sofia = UnaPersona{
    nombre = "Sofia",
    caloriasIngeridas=0,
    nutrientesIngeridos=[]
}

aumentarCalorias::Float->Persona->Persona
aumentarCalorias calorias persona = persona{
    caloriasIngeridas = caloriasIngeridas persona + calorias
}

sumarNutrientes::[Nutriente]->Persona->Persona
sumarNutrientes nutrientes persona = persona{
    nutrientesIngeridos = nutrientesIngeridos persona ++ nutrientes
}

type Comida = Persona->Persona
tomate::Comida
tomate = sumarNutrientes [VitaminaA, VitaminaC]
zanahoria::Comida
zanahoria = sumarNutrientes [VitaminaA,VitaminaC,VitaminaE,VitaminaK]
carne::Comida
carne = aumentarCalorias 241 . sumarNutrientes [Calcio,Hierro]
pan::String->Comida
pan "blanco" = aumentarCalorias 265
pan "integral" = aumentarCalorias 293 . sumarNutrientes [Fibra]
facturas::Comida
facturas persona 
    | estaPipona persona = aumentarCalorias 100 persona
    | otherwise = aumentarCalorias 5000 persona

estaPipona::Persona->Bool
estaPipona = (>2000).caloriasIngeridas

--sofia luego de comer pan "integral"
--  pan "integral" sofia
--UnaPersona {caloriasIngeridas = 293.0, nutrientesIngeridos = [Fibra]}

type ComidaElaborada = [Comida]
ensalada::ComidaElaborada
ensalada = [tomate,zanahoria]
hamburguesa::ComidaElaborada
hamburguesa = [carne,pan "blanco",tomate]

comer::Persona->ComidaElaborada->Persona
comer persona []= persona
comer persona (x:xs) = comer (x persona) xs

type ComidaCompleta = [ComidaElaborada]

comerMenu::ComidaCompleta->Persona->Persona
comerMenu menu persona = foldl comer persona menu

todosSatisfechos::[Persona]->ComidaCompleta->Bool
todosSatisfechos personas menu = todosPipones menu personas || tienen5Nutrientes menu personas

todosPipones menu = all estaPipona.map (comerMenu menu)
tienen5Nutrientes menu = all tiene5oMas.map (comerMenu menu)
tiene5oMas = (>5).length.nutrientesIngeridos

data Fiesta = UnaFiesta {
    nombreFiesta::Nombre,
    menu::ComidaCompleta,
    invitados::[Persona]
}deriving (Show)

fiestaCopada:: Fiesta->Bool
fiestaCopada fiesta = todosSatisfechos (invitados fiesta) (menu fiesta)

colarse::Persona->Fiesta->Fiesta
colarse persona fiesta 
    | fiestaCopada fiesta = agregarInvitado persona fiesta
    | otherwise = fiesta

agregarInvitado persona fiesta = fiesta{
    invitados = persona:(invitados fiesta)
}    

-- carlos::Persona
-- carlos = UnaPersona{
--     nombre="Carlos",
--     caloriasIngeridas = 150,
--     nutrientesIngeridos = []
-- }
-- casamientoDeAnaYRodolfo::Fiesta
-- casamientoDeAnaYRodolfo= UnaFiesta{
--     nombreFiesta = "Casamiento de Ana y Rodo",
--     menu = [hamburguesa,ensalada],
--     invitados = [sofia,carlos]
-- }
-- maria::Persona
-- maria = UnaPersona{
--     nombre = "Maria",
--     caloriasIngeridas = 200,
--     nutrientesIngeridos = []
-- }

--punto 8
--No, sofia no tendra mas calorias, porque su estado no cambia, sofia siempre tendra 0 kcal y [] de nutrientes
--No, ya que la fucion fiestaCopada depende de ota funcion todosSatisfechos que requiere saber de los invitados de la fiesta, por lo tanto si esta fuera infinita el programa se colgaria.

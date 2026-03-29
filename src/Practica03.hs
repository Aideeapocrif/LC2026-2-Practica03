module Practica03 where

--Sintaxis de la logica proposicional
data Prop = Var String | Cons Bool | Not Prop
            | And Prop Prop | Or Prop Prop
            | Impl Prop Prop | Syss Prop Prop
            deriving (Eq)

instance Show Prop where 
                    show (Cons True) = "⊤"
                    show (Cons False) = "⊥"
                    show (Var p) = p
                    show (Not p) = "¬" ++ show p
                    show (Or p q) = "(" ++ show p ++ " ∨ " ++ show q ++ ")"
                    show (And p q) = "(" ++ show p ++ " ∧ " ++ show q ++ ")"
                    show (Impl p q) = "(" ++ show p ++ " → " ++ show q ++ ")"
                    show (Syss p q) = "(" ++ show p ++ " ↔ " ++ show q ++ ")"

p, q, r, s, t, u :: Prop
p = Var "p"
q = Var "q"
r = Var "r"
s = Var "s"
t = Var "t"
u = Var "u"
w = Var "w"
v = Var "v"

{-
FORMAS NORMALES
-}

--Ejercicio 1
fnn :: Prop -> Prop
fnn = undefined


--Ejercicio 2
fnc :: Prop -> Prop
fnc = undefined

{-
RESOLUCION BINARIA
-}

--Sinonimos a usar
type Literal = Prop
type Clausula = [Literal]

--Ejercicio 1
clausulas :: Prop -> [Clausula]
clausulas = undefined

--Ejercicio 2
resolucion :: Clausula -> Clausula -> Clausula
resolucion = undefined

{-
ALGORITMO DE SATURACION
-}

--Ejercicio 1
hayResolvente :: Clausula -> Clausula -> Bool
hayResolvente c1 c2 = length [l | l <- c1, negacion l `elem` c2] == 1

--auxiliares
pertenece :: Eq a => a -> [a] -> Bool
pertenece _ [] = False
pertenece x (y:ys) = x == y || pertenece x ys

negacion :: Literal -> Literal
negacion (Not p) = p
negacion p = Not p

-- Elimina duplicados
unicos :: Eq a => [a] -> [a]
unicos [] = []
unicos (x:xs) = if pertenece x xs then unicos xs else x : unicos xs

-- Comprueba que todos los elementos de una lista estén en la otra
mismoConjunto :: Eq a => [a] -> [a] -> Bool
mismoConjunto l1 l2 = allIn l1 l2 && allIn l2 l1
    where allIn sub conj = all (\x -> pertenece x conj) sub

contarPares :: Clausula -> Clausula -> Int
contarPares [] _ = 0
contarPares (l:ls) c2 =
    if pertenece (negacion l) c2
    then 1 + contarPares ls c2
    else contarPares ls c2

--Ejercicio 2
--Funcion principal que pasa la formula proposicional a fnc e invoca a res con las clausulas de la formula.
saturacion :: Prop -> Bool
saturacion f = saturar (unicos (clausulas (fnc f)))
     where
      saturar s
        | pertenece [] s = False
        | mismoConjunto s nuevaS = True 
        | otherwise = saturar nuevaS
        where
          resolventesNuevos = [resolucion c1 c2 | c1 <- s, c2 <- s, hayResolvente c1 c2]
          nuevaS = unicos (s ++ resolventesNuevos)

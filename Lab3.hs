module Lab3 where
------------------- Estudiante/s -------------------
-- Nombres y apellidos: 
-- Números: 
----------------------------------------------------

import Prelude
import Data.List
import Data.Maybe

----------------------------------------------------------------------------------
-- Formalización del lenguaje y otros elementos
----------------------------------------------------------------------------------
type Var = String
type I = [(Var,Bool)]

data L = V Var | Neg L | Bin L BC L
  deriving (Eq)

data BC = And | Or | Imp | Iff 
  deriving (Show, Eq)

data Clase = Tau | Contra | Conti
  deriving (Show, Eq)

data Consecuencia = [L] :|= L deriving (Show, Eq)   

data Tableau = Conj [L] Tableau
             | Dis  [L] Tableau Tableau 
             | Hoja [L]
  deriving (Eq)
   
top = Bin (V "p") Or  (Neg $ V "p") 
bot = Bin (V "p") And (Neg $ V "p") 

-- 1)
-- Pre: recibe una lista de literales
-- Pos: retorna True si y solo si la lista es consistente, o sea no contiene un par de literales complementarios
esConsistente :: [L] -> Bool
esConsistente [] = True
esConsistente ((V p):xs) = not (map (== (Neg p)) xs)
esConsistente ((Bin l1 c l2):xs) = esConsistente l1 && esConsistente l2


-- 2)
-- Pre: recibe una interpretación dada como lista de asignaciones (no vacía y consistente) 
-- Pos: retorna la interpretación expresada como una conjunción de literales
int2f :: I -> L
int2f (((V p), True):xs) = Bin p And (int2f xs)
int2f (((V p), False):xs) = Bin (Neg (V p)) And (int2f xs)

-- 3)
-- Pre: recibe una fórmula f de LP
-- Pos: retorna el tableau de f
tableau :: L -> Tableau
tableau f = tabAcc[f][]
  where
      tabAcc [] i = Hoja(nub i)
      tabAcc (V v:fs) i = tabAcc fs ((V v):i)
      tabAcc (Neg (V v):fs) i = tabAcc fs ((Neg(V v)):i)
      tabAcc l@(Bin f1 Or f2:fs) i = Dis (l++i) (tabAcc(f1:fs)i) (tabAcc(f2:fs) i)
      tabAcc l@(Bin f1 Imp f2:fs) i = Dis (l++i) (Neg(f1:fs)i) (tabAcc(f2:fs) i)
      tabAcc l@(Bin f1 Iff f2:fs) i = Dis (l++i) (tabAcc(f1:f2:fs) i) (tabAcc(Neg f1:Neg f2:fs) i)
      tabAcc l@(Neg(Bin f1 And f2):fs) i = Dis (i++l) (tabAcc(Neg f1:f2:fs) i) (tabAcc (f1: Neg f2:fs) i)
      tabAcc l@(Neg(Bin f1 Iff f2):fs) i = Dis (i++l) (tabAcc(Neg f1:f2:fs) i) (tabAcc (f1: Neg f2:fs) i)
      tabAcc l@(Bin f1 And f2:fs) i = Conj (i++l) (tabAcc (f1:f2:fs) i)
      tabAcc l@(Neg(Bin f1 Or f2:fs)) i = Conj (i++l) (tabAcc (Neg f1: Neg f2:fs) i)
      tabAcc l@(Neg( Bin f1 Imp f2):fs) i = Conj (i++l) (tabAcc (f1: Neg f2:fs) i)
      tabAcc l@(Neg(Neg f1:fs)) i = Conj (i++l) (tabAcc (f1:fs) i)

-- 4)
-- Pre: recibe una fórmula f de LP
-- Pos: retorna True si y solo si f es sat
sat :: L -> Bool
sat f = undefined
 
 
hojas :: Tableau -> [[L]]
hojas (Hoja i) 
            | esConsistente i = [i]
            |otherwise = []

hojas (Conj n t) = hojas t
hojas (Dis n t1 t2) = hojas t1 ++ hojas t2

faltantes:: L -> I -> [Var]
faltantes f i = filter (\v -> lookup v i == Nothing) (vars f)

combinaciones::[Var] -> [I]
combinaciones [] = [[]]
combinaciones [v:vs] = [(v,b):cs | b <- [False,True], cs <- combinaciones vs]

fai ::[L] -> I 
fai [] = []
fai (V x:xs) = (x,True): fai xs
fai (Neg (V x):xs) = (x,False): fai xs

-- 5)
-- Pre: recibe una fórmula f de LP
-- Pos: retorna una lista con todos los modelos completos de f
-- Recomendación: para imprimirlos los modelos en lineas distintas:
--                ghci> mapM_ print $ modelos f
modelos :: L -> [I]
modelos f = nub (concatMap completar (map fai (hojas(tableau f))))

  where 
    completar i = map sort [cf++i | cf <- combinacion(faltantes i)]

-- 6)
-- Pre: recibe una fórmula f de LP
-- Pos: retorna la clase semántica a la que pertenece f
clasificar :: L -> Clase
clasificar V p = Conti 
clasificar Neg (V p) = Conti
clasificar Bin l1 And l2
| (clasificar l1) == Tau && (clasificar l2) == Tau = Tau
| (clasificar l1) == Cont && (clasificar l2) == Cont = Cont
| (clasificar l1) == Tau && (clasificar l2) != Tau = Conti
clasificar Bin l1 Or l2
| (clasificar l1) == Tau || (clasificar l2) == Tau = Tau
| (clasificar l1) == Cont && (clasificar l2) == Cont = Cont
| (clasificar l1) == Conti || (clasificar l2) != Tau = Conti

-- 7)
-- Pre: recibe una consecuencia
-- Pos: retorna la consecuencia expresada como una fórmula de LP
cons2f :: Consecuencia -> L
cons2f ([] :|= L) = L
cons2f (x:[] :|= L) = Bin x Imp L
cons2f ((x:xs) :|= L) = Bin x And (cons2f (xs :|= L))



-- 8)     
-- Pre: recibe una consecuencia
-- Pos: retorna True si y solo si la consecuencia es válida
valida :: Consecuencia -> Bool
valida ([] :|= L) = False
valida (l :|= L) = 


todoCerrado :: [L] -> Bool
todoCerrado [] = False
todoCerrado []

-- 9)
-- Pre: recibe una fórmula f de LP
-- Pos: retorna f en FND
fnd :: L -> L
fnd f = undefined

-- 10)
-- Pre: recibe una fórmula f de LP
-- Pos: retorna f en FNC
fnc :: L -> L
fnc f = undefined


----------------------------------------------------------------------------------
-- Fórmulas del Lab1 para probar
----------------------------------------------------------------------------------
p = V "p"
q = V "q"
r = V "r"
fa :: L
fa = Bin p And (Neg (Neg q))                   -- (p ∧ ¬¬q)
fb :: L
fb = Bin p And (Bin (Neg q) And (Neg r))       -- (p ∧ ¬q ∧ ¬r)
fc :: L
fc = Bin (Neg (Neg p)) Or (Neg (Bin q And p))  -- (¬¬p ∨ ¬(q ∧ p))
fd :: L
fd = Bin (Neg (Bin r Imp r)) And fc            -- ¬(r ⊃ r) ∧ (¬¬p ∨ ¬(q ∧ p))


----------------------------------------------------------------------------------
-- Algunas funciones auxiliares 
----------------------------------------------------------------------------------
invertir :: L -> L
invertir f = (invertirVar (swapCon (dobleNeg f) And Or))
  where
    invertirVar (V p)= Neg (V p)
    invertirVar (Neg (V p)) = (V p)
    invertirVar (Neg f) = Neg (invertirVar f)
    invertirVar (Bin f1 o f2)  = Bin (invertirVar f1) o (invertirVar f2) 

swapCon :: L -> BC -> BC -> L
swapCon (V p) b1 b2 = V p
swapCon (Neg f) b1 b2 = Neg (swapCon f b1 b2)
swapCon (Bin f1 o f2) b1 b2 | (o == b1) = Bin (swapCon f1 b1 b2) b2 (swapCon f2 b1 b2)
                            | (o == b2) = Bin (swapCon f1 b1 b2) b1 (swapCon f2 b1 b2)
                            | otherwise = Bin (swapCon f1 b1 b2) o (swapCon f2 b1 b2)

vars :: L -> [Var]
vars (V p) = [p]
vars (Neg f) = vars f
vars (Bin f1 _ f2) = nub ((vars f1) ++ (vars f2))

dobleNeg :: L -> L
dobleNeg (V p) = V p
dobleNeg (Neg (Neg f)) = f
dobleNeg (Neg f) = Neg (dobleNeg f)
dobleNeg (Bin f1 o f2) = Bin (dobleNeg f1) o (dobleNeg f2)

deMorgan :: L -> L
deMorgan (V p) = V p
deMorgan (Neg (Bin f1 And f2)) = Bin (deMorgan $ Neg f1) Or  (deMorgan $ Neg f2)
deMorgan (Neg (Bin f1 Or f2))  = Bin (deMorgan $ Neg f1) And (deMorgan $ Neg f2)
deMorgan (Neg f) = Neg (deMorgan f)
deMorgan (Bin f1 o f2) = Bin (deMorgan f1) o (deMorgan f2)


----------------------------------------------------------------------------------
-- Pretty Printing
----------------------------------------------------------------------------------
instance Show L where
  show (V p)           = p
  show (Neg (Neg a))   = "¬" ++ show (Neg a)
  show (Neg (V p))     = "¬" ++ show (V p)
  show (Neg a)         = "¬" ++ show a ++ ""
  show (Bin a And b)   = "(" ++ show a ++ " /\\ " ++ show b ++ ")"
  show (Bin a Or b)    = "(" ++ show a ++ " \\/ " ++ show b ++ ")"
  show (Bin a Imp b)   = "(" ++ show a ++ " --> " ++ show b ++ ")"
  show (Bin a Iff b)   = "(" ++ show a ++ " <-> " ++ show b ++ ")"

instance Show Tableau where
    show = prettyPrintT  

-- Formatear tableau indentado
-- Adaptado de https://stackoverflow.com/a/19442407
prettyPrintT :: Tableau -> String
prettyPrintT t = unlines (prettyPrintAux t)
  where
    prettyPrintAux (Hoja i)       = [show i ++ if esConsistente i then " O" else " X"]
    prettyPrintAux (Conj l t)     = (show l) : prettyPrintSubTree [t]
    prettyPrintAux (Dis  l t1 t2) = (show l) : prettyPrintSubTree [t1,t2]
    --
    prettyPrintSubTree []     = []
    prettyPrintSubTree [t]    = ((pad "'- " "   ") (prettyPrintAux t))
    prettyPrintSubTree (t:ts) = ((pad "+- " "|  ") (prettyPrintAux t)) ++ prettyPrintSubTree ts
    --
    pad first rest = zipWith (++) (first : repeat rest)
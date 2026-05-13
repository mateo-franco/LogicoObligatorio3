{-# OPTIONS_GHC -fno-warn-tabs #-}
module TestLab3 where

import Lab3             -- Nombre del archivo .hs de la solución
import Test.HUnit       -- Acerca de HUnit: https://hackage.haskell.org/package/HUnit
import Data.List

-- Para ejecutar todas las pruebas usar: runTestTT allTests


tau1_sat_should_be_true  = TestCase (assertEqual "sat tau1" True  (sat tau1))
tau2_sat_should_be_true  = TestCase (assertEqual "sat tau2" True  (sat tau2))
con1_sat_should_be_false = TestCase (assertEqual "sat con1" False (sat con1))
con2_sat_should_be_false = TestCase (assertEqual "sat con2" False (sat con2))
cnt1_sat_should_be_true = TestCase (assertEqual "sat cnt1" True (sat cnt1))
cnt2_sat_should_be_true = TestCase (assertEqual "sat cnt2" True (sat cnt2)) 

tau1_mod_should_be_true  = TestCase (assertBool "modelos tau1" $ sameList [[("p",False)],[("p",True)]]  (modelos tau1))
tau2_mod_should_be_true  = TestCase (assertBool "modelos tau2" $ sameList [[("p",False),("q",False)],[("p",False),("q",True)],[("p",True),("q",False)],[("p",True),("q",True)]] (modelos tau2))
con1_mod_should_be_false = TestCase (assertBool "modelos con1" $ sameList [] (modelos con1))
con2_mod_should_be_false = TestCase (assertBool "modelos con2" $ sameList [] (modelos con2))
cnt1_mod_should_be_true = TestCase (assertBool "modelos cnt1" $ sameList [[("p",False),("q",False)],[("p",False),("q",True)],[("p",True),("q",True)]] (modelos cnt1))
cnt2_mod_should_be_true = TestCase (assertBool "modelos cnt2" $ sameList [[("p",False),("q",False),("r",False)],[("p",False),("q",True),("r",False)],[("p",True),("q",True),("r",False)],[("p",True),("q",False),("r",False)],[("p",True),("q",False),("r",True)],[("p",True),("q",True),("r",True)]](modelos cnt2)) 

tau1_tau_should_be_true  = TestCase (assertEqual "clasificar tau1 Tau" Tau  (clasificar tau1))
tau2_tau_should_be_true  = TestCase (assertEqual "clasificar tau2 Tau" Tau  (clasificar tau2))
tau3_tau_should_be_true  = TestCase (assertEqual "clasificar tau3 Tau" Tau  (clasificar tau3))
tau4_tau_should_be_true  = TestCase (assertEqual "clasificar tau4 Tau" Tau  (clasificar tau4))

con1_con_should_be_true  = TestCase (assertEqual "clasificar con1 Contra" Contra (clasificar con1))
con2_con_should_be_true  = TestCase (assertEqual "clasificar con2 Contra" Contra (clasificar con2))
con3_con_should_be_true  = TestCase (assertEqual "clasificar con3 Contra" Contra (clasificar con3 ))
con4_con_should_be_true  = TestCase (assertEqual "clasificar con4 Contra" Contra (clasificar con4))
con5_con_should_be_true  = TestCase (assertEqual "clasificar con5 Contra" Contra (clasificar con5))

cnt1_cnt_should_be_true  = TestCase (assertEqual "clasificar cnt1 Cont" Conti (clasificar cnt1))
cnt2_cnt_should_be_true  = TestCase (assertEqual "clasificar cnt2 Cont" Conti (clasificar cnt2))
cnt3_cnt_should_be_true  = TestCase (assertEqual "clasificar cnt3 Cont" Conti (clasificar cnt3))

val1_val_should_be_true  = TestCase (assertEqual "valida val1" True  (valida val1))
val2_val_should_be_true  = TestCase (assertEqual "valida val2" True  (valida val2))
val3_val_should_be_true  = TestCase (assertEqual "valida val3" True  (valida val3))
val4_val_should_be_true  = TestCase (assertEqual "valida val4" True  (valida val4))
val5_val_should_be_true  = TestCase (assertEqual "valida val5" True  (valida val5))
val6_val_should_be_true  = TestCase (assertEqual "valida val6" True  (valida val6))
val7_val_should_be_true  = TestCase (assertEqual "valida val7" True  (valida val7))
val8_val_should_be_true  = TestCase (assertEqual "valida val8" True  (valida val8))
val9_val_should_be_true  = TestCase (assertEqual "valida val9" True  (valida val9))
val10_val_should_be_true = TestCase (assertEqual "valida val10" True (valida val10))

inv1_val_should_be_false = TestCase (assertEqual "valida inv1" False (valida inv1))
inv2_val_should_be_false = TestCase (assertEqual "valida inv2" False (valida inv2))
inv3_val_should_be_false = TestCase (assertEqual "valida inv3" False (valida inv3))
inv4_val_should_be_false = TestCase (assertEqual "valida inv4" False (valida inv4))
inv5_val_should_be_false = TestCase (assertEqual "valida inv5" False (valida inv5))
inv6_val_should_be_false = TestCase (assertEqual "valida inv6" False (valida inv6))
inv7_val_should_be_false = TestCase (assertEqual "valida inv7" False (valida inv7))

f1_fnd_ok  = TestCase (assertEqual "isFNDof (fnd f1) f1"  True (isFNDof (fnd f1) f1))
f2_fnd_ok  = TestCase (assertEqual "isFNDof (fnd f2) f2"  True (isFNDof (fnd f2) f2))
f3_fnd_ok  = TestCase (assertEqual "isFNDof (fnd f3) f3"  True (isFNDof (fnd f3) f3))
f4_fnd_ok  = TestCase (assertEqual "isFNDof (fnd f4) f4"  True (isFNDof (fnd f4) f4))
f5_fnd_ok  = TestCase (assertEqual "isFNDof (fnd f5) f5"  True (isFNDof (fnd f5) f5))
f6_fnd_ok  = TestCase (assertEqual "isFNDof (fnd f6) f6"  True (isFNDof (fnd f6) f6))
f7_fnd_ok  = TestCase (assertEqual "isFNDof (fnd f7) f7"  True (isFNDof (fnd f7) f7))
f8_fnd_ok  = TestCase (assertEqual "isFNDof (fnd f8) f8"  True (isFNDof (fnd f8) f8))
f9_fnd_ok  = TestCase (assertEqual "isFNDof (fnd f9) f9"  True (isFNDof (fnd f9) f9))
f10_fnd_ok = TestCase (assertEqual "isFNDof (fnd f10) f10" True (isFNDof (fnd f10) f10))
f11_fnd_ok = TestCase (assertEqual "isFNDof (fnd f11) f11" True (isFNDof (fnd f11) f11))
f12_fnd_ok = TestCase (assertEqual "isFNDof (fnd f12) f12" True (isFNDof (fnd f12) f12))

f1_fnc_ok  = TestCase (assertEqual "isFNCof (fnc f1) f1"  True (isFNCof (fnc f1) f1))
f2_fnc_ok  = TestCase (assertEqual "isFNCof (fnc f2) f2"  True (isFNCof (fnc f2) f2))
f3_fnc_ok  = TestCase (assertEqual "isFNCof (fnc f3) f3"  True (isFNCof (fnc f3) f3))
f4_fnc_ok  = TestCase (assertEqual "isFNCof (fnc f4) f4"  True (isFNCof (fnc f4) f4))
f5_fnc_ok  = TestCase (assertEqual "isFNCof (fnc f5) f5"  True (isFNCof (fnc f5) f5))
f6_fnc_ok  = TestCase (assertEqual "isFNCof (fnc f6) f6"  True (isFNCof (fnc f6) f6))
f7_fnc_ok  = TestCase (assertEqual "isFNCof (fnc f7) f7"  True (isFNCof (fnc f7) f7))
f8_fnc_ok  = TestCase (assertEqual "isFNCof (fnc f8) f8"  True (isFNCof (fnc f8) f8))
f9_fnc_ok  = TestCase (assertEqual "isFNCof (fnc f9) f9"  True (isFNCof (fnc f9) f9))
f10_fnc_ok = TestCase (assertEqual "isFNCof (fnc f10) f10" True (isFNCof (fnc f10) f10))
f11_fnc_ok = TestCase (assertEqual "isFNCof (fnc f11) f11" True (isFNCof (fnc f11) f11))
f12_fnc_ok = TestCase (assertEqual "isFNCof (fnc f12) f12" True (isFNCof (fnc f12) f12))


satTests = [TestLabel "Sat 1" tau1_sat_should_be_true, 
            TestLabel "Sat 2" tau2_sat_should_be_true,
            TestLabel "Sat 3" con1_sat_should_be_false,
            TestLabel "Sat 4" con2_sat_should_be_false,
			TestLabel "Sat 5" cnt1_sat_should_be_true,
			TestLabel "Sat 6" cnt2_sat_should_be_true]

modTests = [TestLabel "Modelos 1" tau1_mod_should_be_true, 
            TestLabel "Modelos 2" tau2_mod_should_be_true,
            TestLabel "Modelos 3" con1_mod_should_be_false,
            TestLabel "Modelos 4" con2_mod_should_be_false,
			TestLabel "Modelos 5" cnt1_mod_should_be_true,
			TestLabel "Modelos 6" cnt2_mod_should_be_true] 			
			
tauTests = [TestLabel "Tautologia 1" tau1_tau_should_be_true, 
            TestLabel "Tautologia 2" tau2_tau_should_be_true,
            TestLabel "Tautologia 3" tau3_tau_should_be_true,
            TestLabel "Tautologia 4" tau4_tau_should_be_true]
          
conTests = [TestLabel "Contradiccion 1" con1_con_should_be_true,
            TestLabel "Contradiccion 2" con2_con_should_be_true,
            TestLabel "Contradiccion 3" con3_con_should_be_true,
            TestLabel "Contradiccion 4" con4_con_should_be_true,
            TestLabel "Contradiccion 5" con5_con_should_be_true]
            
cntTests = [TestLabel "Contingencia 1" cnt1_cnt_should_be_true,
            TestLabel "Contingencia 2" cnt2_cnt_should_be_true,
            TestLabel "Contingencia 3" cnt3_cnt_should_be_true]                           

valTests = [TestLabel "Razonamiento valido 1" val1_val_should_be_true, 
            TestLabel "Razonamiento valido 2" val2_val_should_be_true,
            TestLabel "Razonamiento valido 3" val3_val_should_be_true,
            TestLabel "Razonamiento valido 4" val4_val_should_be_true,
            TestLabel "Razonamiento valido 5" val5_val_should_be_true,
            TestLabel "Razonamiento valido 6" val6_val_should_be_true,
            TestLabel "Razonamiento valido 7" val7_val_should_be_true,
            TestLabel "Razonamiento valido 8" val8_val_should_be_true,
            TestLabel "Razonamiento valido 9" val9_val_should_be_true,
            TestLabel "Razonamiento valido 10" val10_val_should_be_true,
            TestLabel "Razonamiento invalido 1" inv1_val_should_be_false, 
            TestLabel "Razonamiento invalido 2" inv2_val_should_be_false,
            TestLabel "Razonamiento invalido 3" inv3_val_should_be_false,
            TestLabel "Razonamiento invalido 4" inv4_val_should_be_false,
            TestLabel "Razonamiento invalido 5" inv5_val_should_be_false,
            TestLabel "Razonamiento invalido 6" inv6_val_should_be_false,
            TestLabel "Razonamiento invalido 7" inv7_val_should_be_false]                      

fndTests = [TestLabel "FND 1"  f1_fnd_ok,
            TestLabel "FND 2"  f2_fnd_ok,
            TestLabel "FND 3"  f3_fnd_ok,
            TestLabel "FND 4"  f4_fnd_ok,
            TestLabel "FND 5"  f5_fnd_ok,
            TestLabel "FND 6"  f6_fnd_ok,
            TestLabel "FND 7"  f7_fnd_ok,
            TestLabel "FND 8"  f8_fnd_ok,
            TestLabel "FND 9"  f9_fnd_ok,
            TestLabel "FND 10" f10_fnd_ok,
            TestLabel "FND 11" f11_fnd_ok,
            TestLabel "FND 12" f12_fnd_ok]


fnCTests = [TestLabel "FNC 1"  f1_fnc_ok,
            TestLabel "FNC 2"  f2_fnc_ok,
            TestLabel "FNC 3"  f3_fnc_ok,
            TestLabel "FNC 4"  f4_fnc_ok,
            TestLabel "FNC 5"  f5_fnc_ok,
            TestLabel "FNC 6"  f6_fnc_ok,
            TestLabel "FNC 7"  f7_fnc_ok,
            TestLabel "FNC 8"  f8_fnc_ok,
            TestLabel "FNC 9"  f9_fnc_ok,
            TestLabel "FNC 10" f10_fnc_ok,
            TestLabel "FNC 11" f11_fnc_ok,
            TestLabel "FNC 12" f12_fnc_ok]
			
-- Todas las pruebas
allTests = TestList $ satTests ++ modTests ++ tauTests ++ conTests ++ cntTests ++ valTests ++ fndTests ++ fnCTests

-- Razonamiento válido    
val1 = [] :|= Bin (V "q") Imp (V "q")
val2 = [Bin (Bin (V "q") And (V "r")) Imp (V "p"), Bin (Neg (V "q")) Or (V "r"), V "q"] :|= Bin (V "p") And (V "q")
val3 = [Bin (Bin (Neg (V "p")) Or (Neg (V "q"))) Or (V "r"), V "p", V "q"] :|= V "r"
val4 = [(V "q")] :|= Bin (V "q") Iff (V "q")
val5 = [Bin (Bin (V "p") Or (V "q")) Imp (V "r"), Neg (V "q")] :|= Bin (V "p") Imp (V "r")
val6 = [(V "p"), Bin (Neg (V "p")) Or (V "q"), V "q"] :|= Bin (V "p") Or (V "q")
val7 = [Bin (Bin (V "p") Imp (V "q")) Imp (V "r")] :|= Bin (Bin (V "p") And (V "q")) Imp (V "r")
val8 = [Bin (Bin (V "p1" ) And (V "p2")) Imp (Bin (V "p3") And (Bin (V "p4") Or (V "p5"))), Neg(V "p3")] :|= Bin (Neg(V "p1")) Or (Neg(V "p2"))
val9 = [Bin (V "p") And (V "q")] :|= Bin (V "p") Imp (V "q")
val10 = [Bin (V "p") And (V "q")] :|= Bin (V "p") And (V "q")

-- Razonamiento inválido  
inv1 = [Bin (Bin (V "p") And (V "q")) Imp (V "r"), V "p", V "q"] :|= Neg (V "r") 
inv2 = [Bin (V "p") Imp (Bin (V "q") And (V "r")), Neg (V "p")] :|= Bin (Neg (V "q")) And (Neg (V "r"))
inv3 = [Bin (Neg (V "p")) Or (Bin (V "q") Or (V "r")), Bin (Neg (V "p")) Or (V "r"), Bin (V "p") Or (Neg (V "q")) ] :|= Bin (Neg (V "q")) And (Neg (V "r"))  
inv4 = [(Bin (V "p") Or (V "q"))] :|= Bin (V "p") Imp (V "q")
inv5 = [(Bin (V "p") Or (V "q"))] :|= Bin (V "p") And (V "q")
inv6 = [(Bin (Neg (V "p")) Imp (Neg (V "q"))), (Bin (V "r") Imp (V "q"))] :|= (Bin (V "p") Imp (V "r"))
inv7 = [(Bin (Neg (V "p")) Imp (V "q")), (Bin (V "q") Imp (Neg (V "r")))] :|= (Bin (V "r") Or (V "p"))

-- Conversión a FND
f1 = (V "p")
f2 = (Neg (V "p"))
f3 = Bin (V "q") Or (V "q")
f4 = Bin (V "q") And (V "q")
f5 = Bin (V "q") Imp (V "q")
f6 = Bin (V "q") Iff (V "q")
f7 = Bin (Bin (V "q") And (V "r")) Or (V "p")
f8 = Bin (Neg (V "q")) Or (V "r")
f9 = Bin (Bin (Neg (V "p")) Or (Neg (V "q"))) Or (V "r")
f10 = Bin (Bin (Bin (V "p") Imp (V "q")) Imp (V "p")) Imp (V "p")
f11 = Bin (Bin (Neg (V "p")) Imp (Neg (V "q"))) Imp (Bin (V "p") Imp (V "q"))
f12 = Bin (Bin (V "p1") And (V "p2")) Or (Bin (V "p3") And (V "p4"))

-- Tautologias
tau1 = Bin (V "p") Imp (V "p")
tau2 = Bin (Bin (Bin (V "p") Imp (V "q")) Imp (V "p")) Imp (V "p")
tau3 = Bin (V "p") Or (Neg (V "p"))
tau4 = Bin (Bin (V "p") And (V "q")) Or (Neg (Bin (V "p") And (V "q")))

-- Contingencias
cnt1 = Bin (V "p") Imp (V "q")
cnt2 = Bin (Bin (Bin (V "p") Imp (V "q")) Imp (V "r")) Imp (V "p")
cnt3 = Bin (V "p") And (Neg (V "q"))

-- Contradicciones
con1 = Bin (V "p") And (Neg (V "p"))
con2 = Bin (Bin (V "p") Or (Neg(V "p"))) Imp (Bin (V "p") And (Neg(V "p"))) 
con3 = Bin (Bin (Bin (Neg(V "p")) Or (V "q")) And (V "r")) And (Bin (Neg(Bin (V "p") Imp (V "q"))) Or (Neg(V "r")))
con4 = Bin (Bin (V "p") Imp (V "q")) And (Bin (V "p") And (Neg(V "q")))
con5 = Bin (Bin (V "p") And (V "q")) And (Bin (Neg(V "p")) Or (Neg(V "q")))

-- Auxiliares sintácticos
isFND :: L -> Bool
isFND = isDisjunction 
  where 
  isDisjunction (Bin a Or b) = isDisjunction a && isDisjunction b
  isDisjunction a            = isClause a

  isClause (Bin a And b) = isClause a && isClause b
  isClause a             = isLiteral a  

  isLiteral (V p)       = True   
  isLiteral (Neg (V p)) = True
  isLiteral _           = False

isFNC :: L -> Bool
isFNC = isConjunction 
  where 
  isConjunction (Bin a And b) = isConjunction a && isConjunction b
  isConjunction a            = isClause a

  isClause (Bin a Or b) = isClause a && isClause b
  isClause a             = isLiteral a  

  isLiteral (V p)       = True   
  isLiteral (Neg (V p)) = True
  isLiteral _           = False
  
-- Auxiliares semánticos
eval :: (Var -> Bool) -> L -> Bool
eval i (V p)     = i p
eval i (Neg a)   = not (eval i a)
eval i (Bin a And b) = (eval i a) && (eval i b)
eval i (Bin a Or  b) = (eval i a) || (eval i b)
eval i (Bin a Imp b) = (eval i a) <= (eval i b) 
eval i (Bin a Iff b) = (eval i a) == (eval i b)

creari :: [(Var, Bool)] -> (Var -> Bool)
creari []         v = error $ show v ++ " indefinida!"
creari ((a,b):ys) v = case v == a of
                        True  -> b
                        False -> creari ys v 

equiv :: L -> L -> Bool
equiv f1 f2 = all (\i -> eval (creari i) f1 == eval (creari i) f2) 
                  (ints $ nub (vars f1 ++ vars f2))
  where 
    ints :: [Var] -> [I]
    ints []     = [[]]
    ints (v:vs) = [[(v,b)] ++ xs | b <- [True,False], xs <- ints vs]
	
sameList :: (Ord a) => [a] -> [a] -> Bool
sameList xs ys = sort xs == sort ys

isFNDof :: L -> L -> Bool
isFNDof f1 f2 = isFND f1 && f1 `equiv` f2

isFNCof :: L -> L -> Bool
isFNCof f1 f2 = isFNC f1 && f1 `equiv` f2
--Jones Marie
--Mercier Tony

{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

import Parser
import Data.Char
import System.IO
import Control.Monad
import Control.Parallel
import System.IO.Unsafe

-- Declaration des types des expressions

type Nom = String

data Expression = Lam Nom Expression
                | App Expression Expression
                | Var Nom
                | Lit Litteral
                    deriving(Show,Eq)

data Litteral = Entier Integer
              | Bool   Bool
                deriving(Show,Eq)

--Analyse syntaxique d'expressions

-- Q1 :

espacesP:: Parser ()

espacesP = unOuPlus (car ' ')   >> reussit ()

-- Q2 :

nomP:: Parser Nom

nomP = unOuPlus (carCond isLower) >>= \s ->
       zeroOuPlus espacesP        >> reussit s

-- Q3 :

varP:: Parser Expression

varP = nomP            >>= \s ->
       reussit (Var s)

-- Q4 :

applique::[Expression] -> Expression

applique [x]      = x
applique (x:y:exps) = applique ((App x y):exps)

-- Q5 :

--exprP:: Parser Expression

--exprP = varP

exprsP::Parser Expression

exprsP = unOuPlus (exprP) >>= \e ->
         reussit (applique e)

-- Q6 :

lambdaP:: Parser Expression

lambdaP = (car '\\')                 >>
          unOuPlus (carCond isLower) >>= \c ->
          espacesP                   >>
          chaine "->"                >>
          espacesP                   >>
          exprsP                     >>= \e ->
          reussit (Lam c e)

-- Q7 :

--exprP:: Parser Expression

--exprP = varP
--      ||| lambdaP

-- Q8 :

exprParentheseeP:: Parser Expression

exprParentheseeP =  (car '(')  >>
                    exprsP     >>= \s ->
                    (car ')')  >>
                    zeroOuPlus espacesP >> reussit s


-- exprP:: Parser Expression
--
-- exprP =   exprParentheseeP
--       ||| varP
--       ||| lambdaP

-- Q9 :

stringToInt::String -> Integer

stringToInt xs = toEnum (go xs 0) :: Integer
                  where go [] acc = acc
                        go (y:ys) acc = go ys (acc*10 + (digitToInt y))

nombreP:: Parser Expression

nombreP = unOuPlus (carCond isDigit) >>= \s ->
          zeroOuPlus espacesP        >> reussit (Lit (Entier (stringToInt s)))

-- Q10 :

stringToBool::String -> Bool

stringToBool "True"  = True
stringToBool "False" = False

booleenP:: Parser Expression

booleenP = (chaine "True" ||| chaine "False") >>= \s ->
            zeroOuPlus espacesP               >> reussit (Lit (Bool (stringToBool s)))

exprP:: Parser Expression

exprP =  exprParentheseeP
     ||| varP
     ||| lambdaP
     ||| nombreP
     ||| booleenP

-- Q11 :

expressionP:: Parser Expression

expressionP = zeroOuPlus espacesP >> exprsP

-- Q12 :

ras::String -> Expression

ras xs = let r = runParser expressionP xs in
          case complet r of
           True -> resultat r
           _    -> error "Erreur d\'analyse"

--Interprétation

--Interprète simple

data ValeurA = VLitteralA Litteral
             | VFonctionA (ValeurA -> ValeurA)

-- Q14 :

instance Show ValeurA where
    show (VFonctionA _) = "λ"
    show (VLitteralA x) = case x of
                            Entier e -> show e
                            Bool b   -> show b


type Environnement a = [(Nom,a)]

-- Q15 :

interpreteA::Environnement ValeurA -> Expression -> ValeurA

interpreteA envs (Lit x)         = VLitteralA x
interpreteA envs (Var nom)       = case lookup nom envs of
                                    Nothing  -> error "not in the environment"
                                    Just val -> val
interpreteA envs (Lam nom expr)  = VFonctionA (\x -> interpreteA ((nom,x):envs) expr)
interpreteA envs (App exp1 exp2) = case interpreteA envs exp1 of
                                    VFonctionA f -> f (interpreteA envs exp2)
                                    _            -> error("first expression is not correct")


-- Quelques primitives

-- Q16 :

negA::ValeurA

negA = VFonctionA (\val -> case val of
                    VLitteralA (Entier x) -> VLitteralA (Entier (negate x))
                    otherwise             -> error "you can't apply neg on that")

-- Q17 :

addA::ValeurA

addA = VFonctionA (\x -> case x of
                    VLitteralA (Entier x') -> VFonctionA(\y -> case y of
                                                VLitteralA (Entier y') -> VLitteralA (Entier (x'+y'))
                                                otherwise              -> error "you can't apply that second argument")
                    otherwise              -> error "you can't apply that first argument")


-- Q18 :

releveBinOpEntierA::(Integer -> Integer -> Integer) -> ValeurA

releveBinOpEntierA op = VFonctionA(\x -> case x of
                                    VLitteralA(Entier x') -> VFonctionA(\y -> case y of
                                                                VLitteralA(Entier y') -> VLitteralA(Entier (x' `op` y'))
                                                                otherwise             -> error "the second argument is not an integer")
                                    otherwise             -> error "the first argument is not an integer")

-- definition de l'Environnement A
envA :: Environnement ValeurA
envA = [ ("neg",   negA)
       , ("add",   releveBinOpEntierA (+))
       , ("soust", releveBinOpEntierA (-))
       , ("mult",  releveBinOpEntierA (*))
       , ("quot",  releveBinOpEntierA quot) ]

-- Q19 :

ifThenElse::ValeurA

ifThenElse = VFonctionA(\b -> case b of
                          VLitteralA(Bool True)  -> VFonctionA(\x -> VFonctionA(\_ -> x))
                          VLitteralA(Bool False) -> VFonctionA(\_ -> VFonctionA(\y -> y))
                          otherwise              -> error "invalid argument")

-- Q20 :

--main :: IO()

--main = do print "Welcome to minilang ! Please write what you need to know and minilang will just answer, if you want to exit just write quit"
--          boucle
--          where boucle = do putStr "minilang> "
--                            hFlush stdout
--                            cmd <- getLine
--                            if cmd == "quit"
--                              then putStrLn "you're exiting minilang"
--                              else do print (interpreteA envA (ras cmd))
--                                      boucle


-- Interprète avec erreurs

data ValeurB = VLitteralB Litteral
             | VFonctionB (ValeurB -> ErrValB)

type MsgErreur = String
type ErrValB   = Either MsgErreur ValeurB

-- Q21 :

instance Show ValeurB where
  show (VFonctionB _)  = "λ"
  show (VLitteralB x) = case x of
                        Entier e -> show e
                        Bool   b -> show b

-- Q22:

interpreteB:: Environnement ValeurB -> Expression -> ErrValB

interpreteB envs (Lit x)         = Right (VLitteralB x)
interpreteB envs (Var nom)       = case lookup nom envs of
                                    Nothing  -> Left ("la variable " ++ nom ++ " n'est pas definie")
                                    Just val -> Right val
interpreteB envs (Lam nom expr)  = Right (VFonctionB (\x -> interpreteB [(nom,x)] expr))
interpreteB envs (App exp1 exp2) = case interpreteB envs exp1 of
                                    Left msg -> Left msg
                                    Right f  -> case f of
                                                  VFonctionB g -> case interpreteB envs exp2 of
                                                                    Left msg -> Left msg
                                                                    Right x  -> g x
                                                  _            -> Left ( show f ++" n'est pas une fonction, application impossible")

-- Q23 :

addB::ValeurB

addB = VFonctionB (\x -> case x of
                          VLitteralB (Entier x') -> Right (VFonctionB (\y -> case y of
                                                              VLitteralB ( Entier y') -> Right (VLitteralB (Entier (x' + y')))
                                                              otherwise             -> Left ((show y) ++ " n'est pas un entier")))
                          otherwise              -> Left ((show x) ++ " n'est pas un entier"))

-- Q24 :

quotB:: ValeurB

quotB = VFonctionB (\x -> case x of
                            VLitteralB (Entier x') -> Right (VFonctionB (\y -> case y of
                                                        VLitteralB (Entier y') -> if y' == 0
                                                                                    then Left "division par zero"
                                                                                    else Right (VLitteralB (Entier (x' `div` y')))
                                                        otherwise              -> Left ((show y) ++ " n'est pas un entier")))
                            otherwise              -> Left ((show x) ++ " n'est pas un entier"))


-- Interprète traçant

data ValeurC = VLitteralC Litteral
             | VFonctionC (ValeurC -> OutValC)

type Trace   = String
type OutValC = (Trace, ValeurC)

-- Q25:

instance Show ValeurC where
  show (VFonctionC _) = "λ"
  show (VLitteralC x) = case x of
                        Entier e -> show e
                        Bool   b -> show b

-- Q26 :

interpreteC::Environnement ValeurC -> Expression -> OutValC

interpreteC envs (Lit x)         = ("",VLitteralC x)
interpreteC envs (Var nom)       = case lookup nom envs of
                                    Just val -> ("",val)
                                    Nothing  -> error "la variable n'est pas dans l'environnement"
interpreteC envs (Lam nom expr)  = ("",VFonctionC (\x -> interpreteC ((nom,x):envs) expr))
interpreteC envs (App exp1 exp2) = case val of
                                     VFonctionC f -> let inter' = interpreteC envs exp2 in
                                                     let aux    = f (snd inter')        in
                                                    (reverse (fst aux ++ trace ++ "."), snd aux)
                                     _            -> error "l'expression n'est pas une fonction"
                                   where trace = fst inter
                                         val   = snd inter
                                         inter = interpreteC envs exp1


-- Q27 :

pingC::ValeurC

pingC = VFonctionC (\x -> ("p",x))


-- Interprète Monadique

data ValeurM m = VLitteralM Litteral
               | VFonctionM (ValeurM m -> m (ValeurM m))

--interpreteM :: Environnement ValeurM -> Expression -> m ValeurM

instance Show (ValeurM m)  where
  show (VFonctionM _) = "λ"
  show (VLitteralM m) = case m of
                        Entier e -> show e
                        Bool   b -> show b

-- Variante simple

data SimpleM v = S v
                  deriving Show

-- Q29:

interpreteSimpleM:: Environnement (ValeurM SimpleM) -> Expression -> SimpleM (ValeurM SimpleM)

interpreteSimpleM envs (Lit x)         = S (VLitteralM x)
interpreteSimpleM envs (Var nom)       = case lookup nom envs of
                                          Nothing  -> error "not in the environment"
                                          Just val -> S val
interpreteSimpleM envs (Lam nom expr)  = S (VFonctionM (\x -> interpreteSimpleM ((nom,x):envs) expr))
interpreteSimpleM envs (App exp1 exp2) = case interpreteSimpleM envs exp1 of
                                          S (VFonctionM f) -> case interpreteSimpleM envs exp2 of
                                                                S val -> f val
                                          _                -> error("first expression is not correct")

instance Monad SimpleM where
  return      = S
  (S v) >>= f = f v

-- pour compiler sur une version plus récente
instance Applicative SimpleM where
     pure  = return
     (<*>) = ap

instance Functor SimpleM where
     fmap  = liftM

-- Q30:

interpreteM :: Monad m => Environnement (ValeurM m) -> Expression -> m (ValeurM m)

interpreteM envs (Lit x)         = return (VLitteralM x)
interpreteM envs (Var nom)       = case lookup nom envs of
                                          Nothing  -> fail $ nom ++ "not in the environment"
                                          Just val -> return val
interpreteM envs (Lam nom expr)  = return (VFonctionM (\x -> interpreteM ((nom,x):envs) expr))
interpreteM envs (App exp1 exp2) = interpreteM envs exp1 >>= \f ->
                                   interpreteM envs exp2 >>= \val            ->
                                   case f of
                                     VFonctionM g -> g val
                                     _            -> fail "first expression is not a function"

type InterpreteM m = Environnement (ValeurM m) -> Expression -> m (ValeurM m)

interpreteS :: InterpreteM SimpleM
interpreteS = interpreteM

-- Q31:

--Pour effectuer les tests

--envM :: Environnement (ValeurM SimpleM)
--envM = [ ("neg",   negM)
--       , ("add",   addM)]


negM::ValeurM SimpleM

negM = VFonctionM (\val -> case val of
                            VLitteralM (Entier x) -> S(VLitteralM (Entier (negate x)))
                            _                    -> error "you can't apply neg on that")

addM::ValeurM SimpleM

addM = VFonctionM (\x -> case x of
                           VLitteralM (Entier x') -> S (VFonctionM(\y -> case y of
                                                          VLitteralM (Entier y') -> S(VLitteralM (Entier (x'+y')))
                                                          _                      -> error "you can't apply that second argument"))
                           _                      -> error "you can't apply that first argument")

-- Variante traçante

data TraceM v = T (Trace, v)
              deriving Show

-- Q32:

instance Monad TraceM where
    return   v    = T ("",v)
    T (t,v) >>= f = case f v of
                      T(t1,b) -> T (t ++ t1,b)

-- pour compiler sur une version plus récente
instance Applicative TraceM where
     pure  = return
     (<*>) = ap

instance Functor TraceM where
     fmap  = liftM

interpreteMT :: InterpreteM TraceM
interpreteMT = interpreteM

pingM :: ValeurM TraceM
pingM = VFonctionM (\v -> T ("p", v))

-- Q33:

interpreteMT' :: InterpreteM TraceM

interpreteMT' envs (Lit x)         = return (VLitteralM x)
interpreteMT' envs (Var nom)       = case lookup nom envs of
                                          Nothing  -> fail $ nom ++ " not in the environment"
                                          Just val -> return val
interpreteMT' envs (Lam nom expr)  = return (VFonctionM (\x -> interpreteMT' ((nom,x):envs) expr))

interpreteMT' envs (App exp1 exp2) = interpreteMT' envs exp1 >>= \ (VFonctionM f)   ->
                                     interpreteMT' envs exp2 >>= \ val ->
                                     case f val of
                                       T (t,b) -> T ("." ++ t,b)

-- Variante avec erreurs

data ErreurM v = Succes v
               | Erreur String
               deriving Show

-- Q34:

instance Monad ErreurM where
    fail e           = Erreur e
    return           = Succes
    (Succes v) >>= f = f v

--pour compiler sur une version plus récente
instance Applicative ErreurM where
     pure  = return
     (<*>) = ap

instance Functor ErreurM where
      fmap  = liftM

-- Q35:

interpreteE :: InterpreteM ErreurM

interpreteE = interpreteM

interpreteE' :: Monad m => InterpreteM m

interpreteE' = interpreteM

-- Cerise sur le gateau

class Monad m => Injectable m t where
    injecte :: t -> ValeurM m

instance Monad m => Injectable m Bool where
    injecte = VLitteralM . Bool

-- Q36:

instance Monad m => Injectable m Integer where
    injecte = VLitteralM . Entier

-- Q37:

instance (Monad m, Injectable m t) => Injectable m (Bool -> t) where
    injecte f = VFonctionM (\val -> case val of
				     VLitteralM (Bool b) -> return $ injecte $ f b)

instance (Monad m, Injectable m t) => Injectable m (Integer -> t) where
    injecte f = VFonctionM ( \val -> case val of
				      VLitteralM (Entier e) -> return $ injecte $ f e)

-- Q38:

envM :: Monad m => Environnement (ValeurM m)
envM = [ ("add",   injecte ((+) :: Integer -> Integer -> Integer))
       , ("soust", injecte ((-) :: Integer -> Integer -> Integer))
       , ("mult",  injecte ((*) :: Integer -> Integer -> Integer))
       , ("quot",  injecte (quot :: Integer -> Integer -> Integer))
       , ("et",    injecte (&&))
       , ("ou",    injecte (||))
       , ("non",   injecte not)
	, ("if",  ifM)
	, ("infst",infst) ]

-- main:: IO()
--
-- main = do print "Welcome to minilang ! Please write what you need to know and minilang will just answer, if you want to exit just write quit"
--           boucle
--           where boucle = do putStr "minilang> "
--                             hFlush stdout
--                             cmd <- getLine
--                             if cmd == "quit"
--                               then putStrLn "you're exiting minilang"
--                               else do print (unsafePerformIO(interpreteM envM (ras cmd)))
--                                       boucle

-- Pousse-café : Interprète parallèle

-- Q40:

ifM::Monad m => ValeurM m

ifM = VFonctionM(\b -> case b of
                         VLitteralM(Bool True)  -> return (VFonctionM(\x -> return (VFonctionM(\_ -> return x))))
                         VLitteralM(Bool False) -> return (VFonctionM(\_ -> return (VFonctionM(\y -> return y))))
                         otherwise              -> error "invalid argument")

infst::Monad m => ValeurM m

infst = VFonctionM (\x -> case x of
			   VLitteralM (Entier x') -> return ( VFonctionM (\y -> case y of
										 VLitteralM (Entier y') -> return (VLitteralM (Bool (x' < y')))
										 _                       -> fail "second argument is not valid"))
			   _                       -> fail "first argument is not valid")

-- Q41:


main:: IO()

main = do print "Welcome to minilang ! Please write what you need to know and minilang will just answer, if you want to exit just write quit"
          boucle
          where boucle = do putStr "minilang> "
                            hFlush stdout
                            cmd <- getLine
                            if cmd == "quit"
                              then putStrLn "you're exiting minilang"
                              else do print (unsafePerformIO(interpreteM envM (ras cmd)))
                                      boucle
--

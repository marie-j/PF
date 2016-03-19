--Mercier Tony
--Jones Marie

--TP3 : L-Systeme et Tortue

import Graphics.Gloss
import Data.List

dessin::Picture
dessin = interpreteMot (((-150,0),0),100,1,pi/3,"F+-") "F+F--F+F"

systeme::Float->Picture
systeme = lsystemeAnime (lsysteme axiomeVonKoch reglesVonKoch) (((-150,0),0),100,1,pi/3,"F+-")

main::IO()
main = animate (InWindow "l-Système" (1000,1000) (0,0)) white systeme
--main = display (InWindow "L-système" (1000, 1000) (0, 0)) white dessin
--main = animate (InWindow "l-Système" (1000,1000) (0,0)) white vonKoch1Anime
--main = animate (InWindow "l-Système" (1000,1000) (0,0)) white vonKoch2Anime
--main = animate (InWindow "l-Système" (1000,1000) (0,0)) white hilbertAnime
--main = animate (InWindow "l-Système" (1000,1000) (0,0)) white dragonAnime
--main = animate (InWindow "l-Système" (1000,1000) (0,0)) white brindilleAnime
--main = animate (InWindow "l-Système" (1000,1000) (0,0)) white broussailleAnime

--Partie 1 : implémentation

type Symbole  = Char
type Mot      = [Symbole]
type Axiome   = Mot
type Regles   = Symbole -> Mot
type LSysteme = [Mot]

--Q1 :

--version recursive
motSuivantRec::Regles->Mot->Mot
motSuivantRec r uns =
  if uns == []
    then []
    else r (head uns) ++ (motSuivantRec r (tail uns))

--version avec liste de compréhension
motSuivantComprehension::Regles->Mot->Mot
motSuivantComprehension r uns = concat ([r x | x <-uns ])

--version avec une fonction du prelude
motSuivantAvecPrelude::Regles->Mot->Mot
motSuivantAvecPrelude r uns =
  concat (map r uns)

--Q2 :
vonKoch::[Char]
vonKoch = ['-','+','F']

axiomeVonKoch :: [Char]
axiomeVonKoch = last vonKoch : []

reglesVonKoch::Symbole->Mot
reglesVonKoch s | s == '-' = ['-']
                | s == '+' = ['+']
                | s == 'F' = ['F','-','F','+','+','F','-','F']

--Q3 :
lsysteme::Axiome->Regles->LSysteme
lsysteme ms r =
  iterate (motSuivantAvecPrelude r) ms

--Partie 2 : La tortue

type EtatTortue = (Point,Float)
type Config     = (EtatTortue,Float,Float,Float,[Symbole])

--Q4 :
etatInitial::Config->EtatTortue
etatInitial (e,_,_,_,_) = e

longueurPas::Config->Float
longueurPas (_,p,_,_,_) = p

facteurEchelle::Config->Float
facteurEchelle (_,_,f,_,_) = f

angle::Config->Float
angle (_,_,_,a,_) = a

symbolesTortue::Config->[Symbole]
symbolesTortue (_,_,_,_,ss) = ss

--Q5 :
avance::Config->EtatTortue->EtatTortue
avance config etat =
  ((x + d*(cos cap), y+ d*(sin cap)),cap)
    where x   = fst (fst etat)
          y   = snd (fst etat)
          cap = snd etat
          d   = longueurPas config

--Q6 :
tourneAGauche::Config->EtatTortue->EtatTortue
tourneAGauche config etat =
  (fst etat,cap + alpha )
    where alpha = angle config
          cap   = snd etat

tourneADroite::Config->EtatTortue->EtatTortue
tourneADroite config etat =
  (fst etat, cap -alpha )
    where alpha = angle config
          cap = snd etat

--Q7 :
filtresSymbolesTortue::Config->Mot->Mot

filtresSymbolesTortue config [] = []
filtresSymbolesTortue config (s:mots) | (intersect [s] symboles) == [] = filtresSymbolesTortue config mots
                                      | otherwise                      = s:filtresSymbolesTortue config mots
                                      where symboles = symbolesTortue config

-- première définition de EtatDessin
--type EtatDessin = (EtatTortue,Path)

--Q8 :
-- interpreteSymbole::Config->EtatDessin->Symbole->EtatDessin

-- interpreteSymbole config ed 'F' = (et,nouveauDessin)
--                                    where et            = avance config (fst ed)
--                                          nouveauDessin = snd ed ++ [fst et]

-- interpreteSymbole config ed s | s == '-'  = (tourneADroite config et,nouveauDessin)
--                               | s == '+'  = (tourneAGauche config et,nouveauDessin)
--                               | otherwise = error "This symbol is not correct"
--                               where et            = fst ed
--                                       nouveauDessin = snd ed


--Q9 :
-- interpreteMot::Config->Mot->Picture

--interpreteMot config mots =
--  line (snd (go config ed valides))
--    where go config ed []       = ed
--          go config ed (s:mots) = go config (interpreteSymbole config ed s) mots
--          ed = (et, [fst et ] )
--          et = etatInitial config
--          valides = filtresSymboleTortue config mots


--Q10 :
lsystemeAnime::LSysteme->Config->Float->Picture

lsystemeAnime lsys config t =
  interpreteMot config (lsys !!( round t `mod` 10))

--pour effectuer les tests demandés

vonKoch1 :: LSysteme
vonKoch1 = lsysteme "F" regles
  where regles 'F' = "F-F++F-F"
        regles  s  = [s]

vonKoch2 :: LSysteme
vonKoch2 = lsysteme "F++F++F++" regles
  where regles 'F' = "F-F++F-F"
        regles  s  = [s]

hilbert :: LSysteme
hilbert = lsysteme "X" regles
    where regles 'X' = "+YF-XFX-FY+"
          regles 'Y' = "-XF+YFY+FX-"
          regles  s  = [s]

dragon :: LSysteme
dragon = lsysteme "FX" regles
    where regles 'X' = "X+YF+"
          regles 'Y' = "-FX-Y"
          regles  s  = [s]

vonKoch1Anime :: Float -> Picture
vonKoch1Anime = lsystemeAnime vonKoch1 (((-400, 0), 0), 800, 1/3, pi/3, "F+-")

vonKoch2Anime :: Float -> Picture
vonKoch2Anime = lsystemeAnime vonKoch2 (((-400, -250), 0), 800, 1/3, pi/3, "F+-")

hilbertAnime :: Float -> Picture
hilbertAnime = lsystemeAnime hilbert (((-400, -400), 0), 800, 1/2, pi/2, "F+-")

dragonAnime :: Float -> Picture
dragonAnime = lsystemeAnime dragon (((0, 0), 0), 50, 1, pi/2, "F+-")

--Q11 :

type EtatDessin = ([EtatTortue],[Path])

--Q12 :

--redefinition de interpreteSymbole

interpreteSymbole::Config->EtatDessin->Symbole->EtatDessin

interpreteSymbole config ed '[' = (head (fst ed):fst ed,snd ed)

interpreteSymbole config ed ']' = (go (fst ed) [],[]:snd ed)
                                  where go (x:ets) acc | x == head ets = x:((reverse acc )++ ets)
                                                       | otherwise     = go ets (x:acc)

interpreteSymbole config ed 'F' = (ets,nouveauxDessins:(snd ed))
                                    where newEt           = avance config (head (fst ed))
                                          ets             = newEt:fst ed
                                          nouveauxDessins = fst newEt:head (snd ed)

interpreteSymbole config ed '+' = (ets,snd ed)
                                    where ets = tourneAGauche config (head (fst ed)):fst ed

interpreteSymbole config ed '-' = (ets,snd ed)
                                    where ets = tourneADroite config (head (fst ed)):fst ed

--redefinition de interpreteMot

interpreteMot::Config->Mot->Picture

interpreteMot config mots =
  pictures (aux (go config ed valides))
    where aux (_,[])            = []
          aux (ets,p:paths)     = line p: aux (ets,paths)
          go config ed []       = ed
          go config ed (s:mots) = go config (interpreteSymbole config ed s) mots
          ed                    = ([ets], [[fst ets]])
          ets                   = etatInitial config
          valides               = filtresSymbolesTortue config mots

--pour effectuer d'autres tests

brindille :: LSysteme
brindille = lsysteme "F" regles
    where regles 'F' = "F[-F]F[+F]F"
          regles  s  = [s]

broussaille :: LSysteme
broussaille = lsysteme "F" regles
    where regles 'F' = "FF-[-F+F+F]+[+F-F-F]"
          regles  s  = [s]

brindilleAnime :: Float -> Picture
brindilleAnime = lsystemeAnime brindille (((0, -400), pi/2), 800, 1/3, 25*pi/180, "F+-[]")

broussailleAnime :: Float -> Picture
broussailleAnime = lsystemeAnime broussaille (((0, -400), pi/2), 500, 2/5, 25*pi/180, "F+-[]")

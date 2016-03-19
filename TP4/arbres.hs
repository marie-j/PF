-- Jones Marie
-- Mercier Tony

-- TP 4 : Des arbres et des couleurs

-- Partie 1 : Arbres binaires
import Data.List
import Test.QuickCheck
import Control.Concurrent (threadDelay)

-- Q1 :

data Arbre couleur valeur = Noeud couleur valeur (Arbre couleur valeur) (Arbre couleur valeur)
                          | Feuille
                              deriving Show

-- Q2 :

mapArbre::(a -> b) -> Arbre c a -> Arbre c b

mapArbre _ Feuille                = Feuille
mapArbre f (Noeud coul val fg fd) = Noeud coul (f val) (mapArbre f fg) (mapArbre f fd)

foldArbre::(a -> b -> b -> b)-> b -> Arbre c a -> b

foldArbre _ v Feuille              = v
foldArbre f v (Noeud _ val fg fd)  = f val (foldArbre f v fg) (foldArbre f v fd)

-- Q3 :

hauteurRec::Arbre c a -> Int

hauteurRec Feuille                  = 0
hauteurRec (Noeud _ _ gauche droit) = 1 + (max (hauteurRec gauche) (hauteurRec droit))

hauteur::Arbre c a -> Int

hauteur Feuille                = 0
hauteur arbre@(Noeud _ _ _ _ ) = max (foldArbre (\_ fg _-> 1 + fg) 0 arbre) (foldArbre (\_ _ fd-> 1 + fd) 0 arbre)

tailleRec::Arbre c a -> Int

tailleRec Feuille                  = 0
tailleRec (Noeud _ _ gauche droit) = 1 + tailleRec(gauche) + tailleRec(droit)

taille:: Arbre c a -> Int

taille Feuille               = 0
taille arbre@(Noeud _ _ _ _) = foldArbre (\_ fg fd -> 1 + fg + fd ) 0 arbre

-- Q4 :

peigneGauche::[(c,a)] -> Arbre c a

peigneGauche []         = Feuille
peigneGauche (n:paires) = Noeud (fst n) (snd n) (peigneGauche paires) Feuille

-- Q5 :
-- Cette fonction vérifie que la longueur de la liste correspond bien à la hauteur de l'arbre construit en peigne gauche de cette même liste

prop_hauteurPeigne::[(c,a)]-> Bool

prop_hauteurPeigne xs =
  length xs == hauteurRec (peigneGauche xs)

-- Q6 :

prop_taillePeigne::[(c,a)]-> Bool

prop_taillePeigne xs =
  length xs == tailleRec (peigneGauche xs)

prop_mapArbre::[(c,a)] -> Bool

prop_mapArbre xs =
  taille arbre == taille (mapArbre (\x -> x) arbre)
    where arbre = peigneGauche xs

-- Q7 :

estCompletRec:: Arbre c a -> Bool

estCompletRec Feuille                            = True
estCompletRec (Noeud _ _ filsGauche filsDroit) | hauteur filsGauche == hauteur filsDroit = estCompletRec filsGauche && estCompletRec filsDroit
                                                 | otherwise = False

estComplet::Arbre c a -> Bool

estComplet Feuille          = True
estComplet arbre@(Noeud _ _ fg fd ) | hauteur fg == hauteur fd = foldArbre (\_ filsGauche filsDroit -> filsGauche && filsDroit) True arbre
                                     | otherwise                = False

-- Q8 :

prop_estCompletPeigne::[(c,a)] -> Bool

prop_estCompletPeigne xs =
  estComplet (peigneGauche xs)

--Les peignes à gauche complet sont : l'arbre contenant une seule feuille , et celui contenant un noeud et deux feuilles
-- Il est possible de les trouver grâce à quickcheck , en effet celui-ci nous indique qu'il échoue dès la 3ème tentative

-- Q9 :

complet::Int -> [(c,a)] -> Arbre c a

complet 0 []     = Feuille
complet h xs    = Noeud (fst x) (snd x) (complet (h-1) (fst ys)) (complet (h-1) (tail(snd ys)))
                    where ys = splitAt n xs
                          x  = head (snd ys)
                          n  = (length xs) `div` 2

-- Un arbre complet d'une hauteur h possède 2**h -1  noeuds et 2**h feuilles

-- Q10 : La fonction demandée est la fonction repeat

myRepeat::a->[a]

myRepeat x = iterate (\y-> y) x

-- Q11 :

nodes::[((),Char)]

nodes = map (\x -> ((),x)) ['a'..]

-- Q12 :

aplatit::Arbre c a -> [(c,a)]

aplatit Feuille = []
aplatit (Noeud coul val filsGauche filsDroit ) = aplatit filsGauche ++ [(coul,val)] ++ aplatit filsDroit

prop_aplatit::Bool
prop_aplatit =
  map snd (aplatit complet4) == "abcdefghijklmno"
    where complet4 = complet 4 (take 15 nodes)

-- Q13 :

element::Eq a => a -> Arbre c a -> Bool

element _ Feuille = False
element x (Noeud _ val fg fd) | x == val  = True
                              | otherwise = element x fg || element x fd

prop_element::Bool
prop_element =
  element 'f' complet4 == elem 'f' (map snd (aplatit complet4))
    where complet4 = complet 4 (take 15 nodes)

-- Partie 2 : Affichage des arbres

-- Q14 :

noeud::(c -> String) -> (a -> String) -> (c,a) -> String

noeud f_c f_v (c,a) =
  f_v a ++ "[color=" ++ f_c c ++ ", fontcolor=" ++ f_c c ++ "]"

-- Q15 :

arcs::Arbre c a -> [(a,a)]


arcs Feuille                                  = []
arcs arbre@(Noeud _ val filsGauche filsDroit) = (go arbre filsGauche) ++ (go arbre filsDroit)
                                          where go (Noeud _ v _ _) Feuille = []
                                                go (Noeud _ v _ _) newtree@(Noeud _ val fg fd) = (v,val):((go newtree fg) ++ (go newtree fd))



-- Q16 :

arc::(a -> String) -> (a,a) -> String

arc f_val (pere,fils) =
  f_val pere ++ " -> " ++ f_val fils

-- Q17 :

dotise::String -> (c -> String) -> (a -> String) -> Arbre c a -> String

dotise nom f_c f_val arbre =
  unlines (("digraph " ++ nom ++ " {"):("node [fontname=\"DejaVu-Sans\", shape=circle]"):(auxs arbre) ++ ["}"])
     where auxs a                         = noeuds a ++ map (arc f_val) (arcs arbre)
           noeuds (Noeud coul val fg fd ) = noeud f_c f_val (coul,val):(noeuds fg ++ noeuds fd)
           noeuds Feuille                 = []


-- Partie 3 : Enfin de la couleur.. !

-- Q18:

elementR::Ord a=> a -> Arbre c a -> Bool

elementR _ Feuille = False
elementR val (Noeud _ v fg fd )  | val == v = True
                                 | val > v  = elementR val fd
                                 | otherwise = elementR val fg

-- Q19 :

data Couleur = R
             | N
              deriving Show

-- Q20 :

equilibre::Couleur -> a -> Arbre Couleur a -> Arbre Couleur a -> Arbre Couleur a

equilibre _ val (Noeud R valg (Noeud R val2 sfg sfd) fdg) fd = Noeud R valg (Noeud N val2 sfg sfd) (Noeud N val fdg fd)
equilibre _ val (Noeud R valg fgg (Noeud R val2 sfg sfd)) fd = Noeud R val2 (Noeud N valg fgg sfg) (Noeud N val sfd fd)
equilibre _ val fg (Noeud R vald (Noeud R val2 sfg sfd) fdd) = Noeud R val2 (Noeud N val fg sfg ) (Noeud N vald sfd fdd)
equilibre _ val fg (Noeud R vald fgd (Noeud R val2 sfg sfd)) = Noeud R vald (Noeud N val fg fgd ) (Noeud N val2 sfg sfd)
equilibre coul val fg fd                                     = Noeud coul val fg fd

-- Q21 :

inserer::Ord a => a-> Arbre Couleur a -> Arbre Couleur a

inserer x arbre = go newarbre
                    where go (Noeud _ val fg fd) = Noeud N val fg fd
                          newarbre = auxInserer x arbre
                          auxInserer x Feuille                                                       = Noeud R x Feuille Feuille
                          auxInserer x tree@(Noeud coul val filsGauche filsDroit) | elementR x tree = tree
                                                                                  | x < val  = equilibre coul val (auxInserer x filsGauche) filsDroit
                                                                                  | x > val  = equilibre coul val filsGauche (auxInserer x filsDroit)

-- Q22 :

--pour effectuer des tests sur les arbres colorés

colorful_nodes::[a] -> [(Couleur,a)]

colorful_nodes = map (\x -> (N,x))

-- definition de proprietes

prop_tailleRetN::Int -> Bool

prop_tailleRetN h =
  taille (inserer 0 colorful_tree ) == (taille colorful_tree + 1)
    where colorful_tree = peigneGauche (take h (colorful_nodes [1..]))

prop_aplatissement::[Int]->Bool

prop_aplatissement xs =
    map snd (aplatit (colorful_tree xs)) == nub (sort xs)
      where colorful_tree (x:ys) = inserer x (colorful_tree ys)
            colorful_tree []     = Feuille

-- Q23 :
-- Ici le type a devient Char puisque on supposera que l'on passe TOUJOURS la liste xs sous forme de chaîne de caractères

arbresDot::[Char] -> [String]

arbresDot [] = []
arbresDot xs = map (dotise "arbre" f_c f_val) (go xs Feuille)
                    where f_c c   = case c of
                                     R -> "red"
                                     N -> "black"
                          f_val v = v:""
                          go []     _     = []
                          go (x:ys) arbre = let newarbre = inserer x arbre in
                                              newarbre:go ys newarbre

-- pour tester

main::IO()

main = mapM_ ecrit arbres
    where ecrit a = do writeFile "arbre2.dot" a
                       threadDelay 1000000
          arbres  = arbresDot' "gcfxieqzrujlmdoywnbakhpvst"


--Extensions

--Extension 1 :

elementR'::(a -> a -> Ordering) -> a -> Arbre c a -> Bool

elementR' _ _ Feuille                = False
elementR' f val (Noeud _ v fg fd )   = case f val v of
                                        LT -> elementR' f val fg
                                        EQ -> True
                                        GT -> elementR' f val fd

inserer'::(a -> a -> Ordering) -> a -> Arbre Couleur a -> Arbre Couleur a

inserer' f x arbre = go newarbre
                      where go (Noeud _ val fg fd) = Noeud N val fg fd
                            newarbre = auxInserer f x arbre
                            auxInserer _ x Feuille = Noeud R x Feuille Feuille
                            auxInserer f x tree@(Noeud coul val filsGauche filsDroit) = case f x val of
                                                                                          LT -> equilibre coul val (auxInserer f x filsGauche) filsDroit
                                                                                          GT -> equilibre coul val filsGauche (auxInserer f x filsDroit)
                                                                                          EQ -> tree

arbresDot'::[Char] -> [String]

arbresDot' [] = []
arbresDot' xs = map (dotise "arbre" f_c f_val) (go cmp xs Feuille)
                where f_c c   = case c of
                                    R -> "red"
                                    N -> "black"
                      f_val v = v:""
                      go _ [] _     = []
                      go f (x:ys) arbre = let newarbre = inserer' f x arbre in
                                          newarbre:go f ys newarbre
                      cmp x y = compare x y

--Extension 2 :
type Dictionnaire clé valeur = Arbre Couleur (clé,valeur)

recherche::Ord a => a -> Dictionnaire a b -> Maybe b

recherche x Feuille                               = Nothing
recherche x (Noeud _ (cle,val) fg fd) | x == cle  = Just val
                                      | x < cle   = recherche x fg
                                      | otherwise = recherche x fd


insererDico::Ord a => (a,b) -> Dictionnaire a b -> Dictionnaire a b

insererDico x dico = go newarbre
                              where go (Noeud _ v fg fd) = Noeud N v fg fd
                                    newarbre             = auxInserer x dico
                                    auxInserer x Feuille = Noeud R x Feuille Feuille
                                    auxInserer x@(cle,val) d@(Noeud c v@(c',_) fg fd) = case recherche cle d of
                                                                                    Just _  -> d
                                                                                    Nothing -> if (cle < c') then equilibre c v (auxInserer x fg) fd
                                                                                                             else equilibre c v fg (auxInserer x fd)

--Mercier Tony
--Jones Marie

--TP2 PF : Partie 1 Echauffement

alterne::[a]->[a]
alterne xs | xs == [] = []
           | tail xs == [] = [(head xs)]
           | otherwise = [(head xs)] ++ (alterne (tail (tail xs)))

combine::(a->a)->[a]->[a]->[a]
combine f xs ys | xs == [] = []
                | ys == [] = []
                | otherwise = [(f (head xs) (head ys))] ++ (combine f (tail xs) (tail ys))

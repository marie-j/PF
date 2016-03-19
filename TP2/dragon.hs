--Mercier Tony
--Jones Marie

--TP2 PF : Partie 3 Dragon

--import Graphics.Gloss

--main = animate (InWindow "Dragon" (500, 500) (0, 0)) white (dragonAnime (50,250) (450,250))

--dragonAnime a b t = Line (dragonOrdre a b (round t `mod` 20))

pointAintercaler::Point->Point->Point
pointAintercaler x y =
  let a = fst x in
  let b = snd x in
  let c = fst y in
  let d = snd y in
  ((a+c)/2 + (d-b)/2, (b+d)/2 +(a-c)/2)

aux::Path->Int->Path
aux xs acc | length xs < 2 = []
           | mod acc 2 == 0 = [(pointAintercaler (head xs) (head (tail xs)))] ++ (aux (tail xs) (acc+1))
           | otherwise = [(pointAintercaler (head (tail xs)) (head xs))] ++ (aux (tail xs) (acc+1))

pasDragon::Path->Path
pasDragon ds =
  ds ++ (aux ds 0)

dragon::Point->Point->Path
dragon x y =
  last(take 10 (iterate pasDragon [x,y]))

dragonOrdre::Point->Point->Int->Path
dragonOrdre x y n =
  if n == 0
    then [x,y]
    else let c = pointAintercaler x y in
       dragonOrdre x c (n-1) ++ dragonOrdre c y (n-1)

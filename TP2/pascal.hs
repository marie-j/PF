--Mercier Tony
--Jones Marie

--TP2 PF : Partie 2 Pascal

pasPascal::[a]->[a]
pasPascal xs =
  let ys = [0] ++ xs  in
  (zipWith (+) xs ys) ++ [1]

pascal::[[a]]
pascal =
  iterate (pasPascal) [1]

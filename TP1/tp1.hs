--Q3
sommeDeXaY x y =
  if x> y
  then 0
  else x + sommeDeXaY (x+1) y

--Q4
somme xs =
  if xs == []
  then 0
  else head xs + somme (tail xs )

--Q5

last' xs =
  if length xs == 1
  then head xs
  else last (tail xs)

init' xs =
  if length xs == 1
  then []
  else reverse (tail (reverse xs))

--Q6
nthElement xs n | n == 0 = head xs
                | otherwise = nthElement (tail xs) (n-1)

append xs ys | xs == [] = ys
             | ys == [] = xs
             | otherwise = append (init' xs) ((last xs) : ys)

turnToOneList xss | xss == [] = []
                  | tail xss == [] = head xss
                  | otherwise = append (head xss) (turnToOneList (tail xss))

applyFunction f xs | xs == [] = []
                   | otherwise = append [f (head xs)] (applyFunction f (tail xs))

--Q7 : x représente une fonction qui prend en paramètre un int i et qui retourne le ième élément de la liste

--Q8
myLength xs =
  let f x = 1 in
    somme (applyFunction f xs)

--Q9
power f x n =
  if n == 0
  then [x]
  else take (n+1) (iterate f x)

recPower f x n =
  if n == 0
  then [x]
  else append [x] (recPower f (f x) (n-1))

--Q10
createList n =
  let f x = (x+1) in
    power f 0 n
    

-- TP1
-- Mercier Tony, Jones Marie

-- Question 3
sommeDeXaY :: (Enum a, Num a) => a -> a -> a
sommeDeXaY x y = sum[x..y]

-- Question 4
somme :: Num a => [a] -> a
somme [] = 0
somme (x:xs) = x + somme xs

-- Question 5
last' :: [a] -> a
last' xs =  head (reverse xs)

init' :: [a] -> [a]
init' xs = take (length xs - 1) xs

-- Question 6
(+++) :: [a] -> [a] -> [a]
(+++) [] [] = []
(+++) [] (y:ys) = y : ([] +++ ys)
(+++) (x:xs) ys = x : (xs +++ ys)

(!!!) :: [a] -> Int -> a
(!!!) (x:xs) y
      | y == 0 = x
      | otherwise = (!!!) xs (y-1)

concat' :: [[a]] -> [a]
concat' [x] = x
concat' (x:xs) = (+++) x (concat' xs)

map' :: (a -> b) -> [a] -> [b]
map' f [] = []
map' f (x:xs) = f x : map' f xs

-- Question 7
-- Lorsque l'on utilisera x avec un paramètre entier, on aura accès aux ième élément de la liste contenu dans x

-- Question 8
longueur :: Num a => [t] -> a
longueur xs = let one x = 1 in somme (map' one xs)

-- Question 9
one x = 1
build :: (a -> a) -> a -> Int -> [a]
build f x n = take n (iterate f x)

build' :: (a -> a) -> a -> Int -> [a]
build' f x 0 = []
build' f x n = x : build' f (f x) (n-1)

-- Question 10
entierConsecutif :: Int -> [Int]
entierConsecutif n = let addOne x = x + 1 in build addOne 0 (n+1)

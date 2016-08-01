import Data.List
qsort :: (Ord a)=> [a]-> [a]
qsort [] = []
qsort (pivot:rest) = qsort lower ++ [pivot] ++ qsort upper
                     where lower = [x | x<-rest, x<= pivot]
                           upper = [x | x<-rest, x > pivot]


ssort ::(Ord a)=> [a] ->[a]
ssort [] = []
ssort xs = m: ssort (delete m xs)
           where m = minimum xs
           
split::(Ord a) => [a] ->a ->[a] ->[a]
split [] m r= m:(ssort' r)
split (x:xs) m r = if x < m
                   then split xs x (m:r)
                   else split xs m (x:r)
                   
ssort' [] = []
ssort' (x:xs) = split xs x []

inserts ::(Ord a) => a->[a] ->[a]
inserts x xs = takeWhile ((<=)x) xs ++ [x] ++ dropWhile ((<=)x) xs
isort' xs = foldr inserts [] xs


sum' [] = 0
sum' (x:xs) = x + sum' xs
product' [] = 1
product' (x:xs) = x * product' xs

foldr' f i [] = i
foldr' f i (x:xs) = f i (foldr' f i xs)

{-
--graph
mkGraph :: (Ix n Num w) => Bool->(n,n)->[(n,n,w)] -> (Graph n w)

adjacent :: (Ix n, Num w) => (Graph n w) -> n -> [n]
nodes :: (Ix n, Num w) => (Graph n w) -> [n]



data Graph n w = Vertex n [((Graph n w), w)]

-}

main :: IO()
main = do
    print $ removeFirst 5 [5, 1, 5, 3, 5] == [1, 5, 3, 5]
    print $ removeFirst 3 [5, 1, 5, 3, 5] == [5, 1, 5, 5]

removeFirst :: (Eq a) => a -> [a] -> [a]
removeFirst _ [] = [] 
removeFirst el (x:xs)
 |el == x = xs
 |otherwise = x : removeFirst el xs
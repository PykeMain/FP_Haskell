import Data.List

main :: IO()
main = do
    print $ isSorted [-5, -5, -6] == True
    print $ isSorted [-5, -5, -4] == True
    print $ isSorted [1, 1, 1, 1, 1, 1, 1, 1, 1] == True
    print $ isSorted [1, 2, 3, 3, 3, 4, 5, 6, 6] == True
    print $ isSorted [1, -1, -3, -3, -3, -4, -5, -6, -6] == True
    print $ isSorted [1, 2, 3, 3, 3, 4, 5, 6, 5] == False
    print $ isSorted [-100, -99, -99, -99] == True
    print $ isSorted [-100, -99, -99, -99, 100] == True
    print $ isSorted [100, 101, -102] == False
    print $ isSorted [1, 2, 3, 4, 5, 6] == True
    print $ isSorted [-1, -2, -3, -4, -5, -6] == True
    print $ isSortedXs [-5, -5, -6] == True
    print $ isSortedXs [-5, -5, -4] == True
    print $ isSortedXs [1, 1, 1, 1, 1, 1, 1, 1, 1] == True
    print $ isSortedXs [1, 2, 3, 3, 3, 4, 5, 6, 6] == True
    print $ isSortedXs [1, -1, -3, -3, -3, -4, -5, -6, -6] == True
    print $ isSortedXs [1, 2, 3, 3, 3, 4, 5, 6, 5] == False
    print $ isSortedXs [-100, -99, -99, -99] == True
    print $ isSortedXs [-100, -99, -99, -99, 100] == True
    print $ isSortedXs [100, 101, -102] == False
    print $ isSortedXs [1, 2, 3, 4, 5, 6] == True
    print $ isSortedXs [-1, -2, -3, -4, -5, -6] == True

isSortedXs :: [Int] -> Bool
isSortedXs xs = sort xs == xs || reverse xs == sort xs

isSorted :: [Int] -> Bool
isSorted [] = True
isSorted [x1] = True
isSorted (x1 : x2 : xs)
 |x1 == x2 = isSorted (x2 : xs)
 |x1 > x2 = descending (x2 : xs)
 |x1 < x2 = ascending (x2 : xs)

descending :: [Int] -> Bool
descending [] = True
descending [x1] = True
descending (x1 : x2 : xs)
 | x1 >= x2 = descending (x2 : xs)
 | otherwise = False

ascending :: [Int] -> Bool
ascending [] = True
ascending [x1] = True
ascending (x1 : x2 : xs)
 | x1 <= x2 = ascending (x2 : xs)
 |otherwise = False

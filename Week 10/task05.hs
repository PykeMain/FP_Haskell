main :: IO()
main = do
    print $ combine [(1, 2), (1, 2)] == (11, 22)
    print $ combine [(3, 9), (8, 7), (7, 9), (8, 8), (5, 0), (9, 2)] == (377802, 989859)

type Tuples = (Int, Int)

combine :: [Tuples] -> Tuples
combine [] = (error "Not enough elements")
combine ((u, d) : uds) = helper uds (show $ min u d) (show $ max u d)
 where 
    helper :: [Tuples] -> [Char] -> [Char] -> Tuples
    helper [] little big = (read little, read big)
    helper ((u, d) : uds) little big = helper uds (little ++ (show $ min u d)) (big ++ (show $ max u d))
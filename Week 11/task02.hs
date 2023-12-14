main :: IO()
main = do
    print $ controller "" == ""
    print $ controller ".........." == "0000000000"
    print $ controller "P...." == "12345"
    print $ controller "P.P.." == "12222"
    print $ controller "..P...O..." == "0012343210"
    print $ controller "P......P......" == "12345554321000"
    print $ controller "P.P.P...." == "122234555"
    print $ controller ".....P.P........P...." == "000001222222222234555"
    print $ controller ".........." == "0000000000"
    print $ controller "P.." == "123"
    print $ controller "P...." == "12345"
    print $ controller "P......P......" == "12345554321000"
    print $ controller "P.P.." == "12222"
    print $ controller "P.P.P...." == "122234555"
    print $ controller ".....P.P........P...." == "000001222222222234555"
    print $ controller ".....P......P.P..P...." == "0000012345554333321000"
    print $ controller "P.O...." == "1210000"
    print $ controller "P......P.O...." == "12345554345555"
    print $ controller "P..OP..P.." == "1232222100"
    print $ controller "P......P..OP..P..." == "123455543233334555"
    print $ controller "..P...O....." == "001234321000"

controller :: String -> String
controller xs = positions xs [] 0 0
 where
    positions :: String -> [Int] -> Int -> Int -> String
    positions [] result _ _ = reverse $ concat $ map (show) result
    positions (i:inputs) [] _ _
     |i == '.' = positions inputs [0] 0 0
     |i == 'P' = positions inputs [1] 5 1
     |otherwise = error "Not an input"
    positions (i:inputs) (r:result) wanted delta
     |i == '.' && r + delta <= 5 && r + delta >= 0 = positions inputs (r + delta : r : result) wanted delta
     |i == 'P' && r == 0 = positions inputs (r + 1 : r : result) 5 1
     |i == 'P' && r == 5 = positions inputs (r - 1 : r : result) 0 (-1)
     |i == 'P' && delta /= 0 = positions inputs (r : r : result) wanted 0
     |i == 'P' && wanted == 5 = positions inputs (r + 1 : r : result) wanted 1
     |i == 'P' = positions inputs (r - 1 : r : result) wanted (-1)
     |i == 'O' = positions inputs (r - delta : r : result) (if wanted == 5 then 0 else 5) (-delta) 
     |otherwise =  positions inputs (r : r : result) wanted delta

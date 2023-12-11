import Data.List
import Data.Char

main :: IO()
main = do
    {--
    print $ findMaxPalindrome 1112332 -- == 3211123
    print $ findMaxPalindrome 22220 -- == 22022
    print $ findMaxPalindrome 2205 -- == 252
    print $ findMaxPalindrome 120021 -- == 210012
    print $ findMaxPalindrome 12320 -- == 232
    print $ findMaxPalindrome 123 -- == 3
    --}
    print $ calculate "1+2+x" [('x', 5)] == 8
    print $ calculate "x+2+x-2+y+z" [('x', 1), ('y', 2), ('z', 3)] == 7
    print $ calculate "x+2-x-2+y+x" [('x', 1), ('y', -15)] == (-14)
    print $ calculate "y+2+x-2+z+z+z+x+5" [('x', 1), ('y', 2), ('z', 3)] == 18
    print $ calculate "8-2" [] == 6
    print $ calculate "5" [] == 5

{--
createPalindrome xs = (map (take 1) (filter (odd . length) xs)) ++ (filter (even . length) xs)

findMaxPalindrome  =  group . sort . show
--}

type Pairing = (Char, Int)

swapper :: Char -> [Pairing] -> Int
swapper x values
 |x >= '0' && x <= '9' = digitToInt x
 |otherwise = helper x values
  where
    helper :: Char -> [Pairing] -> Int
    helper _ [] = (error "Not given var")
    helper x ((y,v):values)
     |x == y = v
     |otherwise = helper x values

calculator :: String -> [Pairing] -> Int -> Int
calculator [] _ result = result
calculator (p:x:procxs) values result
 |p == '+' = calculator procxs values (result + swapper x values)
 |p == '-' = calculator procxs values (result - swapper x values)
 |otherwise = (error "Not a procedure")

calculate :: String -> [Pairing] -> Int
calculate [] _ = 0
calculate (x:procxs) values = calculator procxs values (swapper x values)
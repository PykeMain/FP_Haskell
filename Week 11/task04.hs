import Data.Char

main :: IO()
main = do
    print $ squareDigits 9119 == 811181
    print $ squareDigits (-9119) == -811181

squareDigits :: Int -> Int
squareDigits n = div n (abs n) * read (helper (show (abs n)))
 where
    helper :: String -> String
    helper [] = []
    helper (d : digit) = show (digitToInt d * digitToInt d) ++ helper digit
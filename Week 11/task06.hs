main :: IO()
main = do
    print $ matching "1234" -- == []
    print $ matching ",[.[-],]" -- == [(3,5),(1,7)]
    print $ matching ",+[-.,+]" -- == [(2,7)]
    print $ matching "[][]" -- == [(0,1),(2,3)]

matching :: String -> [(Int, Int)]
matching xs = helper (brackets xs) [] []
 where
    helper :: [(Int, Char)] -> [(Int, Char)] -> [(Int, Int)] -> [(Int, Int)]
    helper [] _ result = result
    helper ((i, b): leftover) [] result
     |b == '[' = helper leftover [(i,b)] result
     |otherwise = error "Starts with an closing bracket. HUH"
    helper ((i, b): leftover) ((iStack, bStack):stack) result
     |b == '[' = helper leftover ((i,b):(iStack, bStack):stack) result
     |otherwise = helper leftover stack ((iStack, i) : result)

brackets :: String -> [(Int, Char)]
brackets = filter ((\ x -> elem x ['[', ']']) . snd) . zip [0 ..]
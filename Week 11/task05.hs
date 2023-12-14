main :: IO()
main = do
    print $ stocklist stocks ['A','B'] -- == [('A',200),('B',1140)]
    print $ stocklist stocks ['C','X'] -- == [('C',500),('X',0)]
    print $ stocklist stocks ['Y','X'] -- == [('Y',0),('X',0)]
    print $ stocklist stocks ['C'] -- == [('C',500)]
     where
        stocks = [Stock "ABAR" 200, Stock "CDXE" 500, Stock "BKWR" 250, Stock "BTSQ" 890, Stock "DRTY" 600]

data Stock = Stock String Int
 deriving (Show)

stocklist :: [Stock] -> [Char] -> [(Char,Int)]
stocklist list chars = map (\ ch -> (ch, sum ( map (\ (Stock (n:name) c) -> c) (filter (\ (Stock (n:name) c) ->  (n == ch)) list)))) chars
import Common (Hand, HandType, Card (..), handType, solve)

allCards :: [Card]
allCards = [A,K,Q,T] ++ map Numeric [2..10]

possibleHands :: Hand -> [Hand]
possibleHands (J:cs) = [c:h | c <- allCards, h <- possibleHands cs]
possibleHands (c:cs) = map (c:) (possibleHands cs)
possibleHands []     = [[]]

compareHands :: Hand -> Hand -> Ordering
compareHands [] []                  = EQ
compareHands (x:xs) (y:ys) | x == y = compareHands xs ys
compareHands (J:_) _                = LT
compareHands _ (J:_)                = GT
compareHands (x:_) (y:_)            = compare x y

handType' :: Hand -> HandType
handType' = maximum . map handType . possibleHands

main :: IO ()
main = interact (solve compareHands handType')

module Common where

import Data.List (sort, group, sortBy)
import Data.Function (on)

type Hand = [Card]
data Card = Numeric Int | T | J | Q | K | A
    deriving (Show, Eq, Ord)
data HandType = HighCard | Pair | TwoPairs | Three | House | Four | Five
    deriving (Show, Eq, Ord)

fromChar :: Char -> Card
fromChar 'A' = A
fromChar 'K' = K
fromChar 'Q' = Q
fromChar 'J' = J
fromChar 'T' = T
fromChar c   = Numeric (read [c])

parseHandAndBid :: String -> (Hand, Int)
parseHandAndBid s = (map fromChar $ take 5 s, read $ drop 6 s)

handType :: Hand -> HandType
handType cs
    | length (head sorted) == 5 = Five
    | length (head sorted) == 4 = Four
    | length (head sorted) == 3 && length sorted == 2 = House
    | length (head sorted) == 3 && length sorted == 3 = Three
    | length (head sorted) == 2 && length sorted == 3 = TwoPairs
    | length sorted == 4 = Pair
    | otherwise = HighCard
    where
        grouped = group $ sort cs
        sorted = sortBy (compare `on` negate . length) grouped

compareHands :: (Hand -> Hand -> Ordering) -> (Hand -> HandType) -> Hand -> Hand -> Ordering
compareHands cmp ht a b
    | ht a > ht b = GT
    | ht a < ht b = LT
    | otherwise   = cmp a b

solve :: (Hand -> Hand -> Ordering) -> (Hand -> HandType) -> String -> String
solve cmp ht s = show winnings
    where
        hands = map parseHandAndBid $ filter (not . null) $ lines s
        sortedHands = sortBy (compareHands cmp ht `on` fst) hands
        winnings = sum $ zipWith (\(_, bet) rank -> rank * bet) sortedHands [1..]

{-# LANGUAGE TupleSections #-}
import Data.List (find, isInfixOf)
import Data.Maybe (isJust)
import Debug.Trace (trace)

t x = trace (show x) x

dropHeader :: [String] -> [String]
dropHeader = drop 2

breakNumbers :: [String] -> ([Int], [Int])
breakNumbers xs = (map read mine, map read winning)
    where (mine, winning) = drop 1 <$> break (== "|") xs

parseLine :: String -> ([Int], [Int])
parseLine = breakNumbers . dropHeader . words

parseInput :: String -> [([Int], [Int])]
parseInput = map parseLine . lines

numberOfMatches :: ([Int], [Int]) -> Int
numberOfMatches = go
    where
        go (x:xs, winning)
            | x `elem` winning = 1 + go (xs, winning)
            | otherwise        = go (xs, winning)
        go _                 = 0

solve :: [([Int], [Int])] -> Int
solve = solve' . map (1,)

solve' :: [(Int, ([Int], [Int]))] -> Int
solve' []                 = 0
solve' ((count, card):xs) = count + solve' xs'
    where
        matches = numberOfMatches card
        xs' = copyCards matches xs
        copyCards 0 cs             = cs
        copyCards _ []             = []
        copyCards n ((c, card):cs) = (c + count, card) : copyCards (n-1) cs

main :: IO ()
main = interact (show . solve . parseInput)
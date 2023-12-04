import Data.List (find, isInfixOf)
import Data.Maybe (isJust)

dropHeader :: [String] -> [String]
dropHeader = drop 2

breakNumbers :: [String] -> ([Int], [Int])
breakNumbers xs = (map read mine, map read winning)
    where (mine, winning) = drop 1 <$> break (== "|") xs

parseLine :: String -> ([Int], [Int])
parseLine = breakNumbers . dropHeader . words

parseInput :: String -> [([Int], [Int])]
parseInput = map parseLine . lines

solve :: ([Int], [Int]) -> Int
solve = go 0
    where
        go n (x:xs, winning)
            | x `elem` winning = go (if n == 0 then 1 else 2*n) (xs, winning)
            | otherwise        = go n (xs, winning)
        go n _                 = n

main :: IO ()
main = interact (show . sum . map solve . parseInput)
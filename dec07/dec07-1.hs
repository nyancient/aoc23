import Common (handType, solve)

main :: IO ()
main = interact (solve compare handType)

import Data.List

isUnique x          = (length x) == (length $ nub x)
isLetterUnique x    = (length x) == (length $ nub $ (map sort) $ x)

main = do
    str             <- readFile "input_day4.txt"
    let input       = map words $ lines str

    putStr "Part 1: "
    putStrLn $ show $ length $ filter isUnique input

    putStr "Part 2: "
    putStrLn $ show $ length $ filter isLetterUnique input

import Data.List

isUnique x = (length x) == (length $ nub x)
isLetterUnique x = (length x) == (length $ nub $ (map sort) $ x)

main = do
    str <- readFile "input_day4.txt"
    putStrLn $ show $ length $ filter atMostOneAnagram $ filter isUnique $ map words $ lines str

import Data.List
import Data.Maybe

redistribute curr = map (uncurry addTo) (zip [0..] curr)
    where
        len                     = length curr
        amount                  = maximum curr
        index                   = head $ elemIndices amount curr
        order                   = [index + 1 .. len - 1] ++ [0 .. index]
        addToAll                = amount `div` len
        addOneTo                = take (amount `mod` len) order
        addTo i x
            | i == index        = addToAll
            | i `elem` addOneTo = x + addToAll + 1
            | otherwise         = x + addToAll

countRedistributions history prev
    | curr `elem` history       = 1
    | otherwise                 = 1 + (countRedistributions (prev : history) curr)
    where
        curr                    = redistribute prev

countCycles history prev
    | isJust loop               = 2 + fromJust loop
    | otherwise                 = countCycles (prev : history) curr
    where
        curr                    = redistribute prev
        loop                    = elemIndex curr history

main :: IO ()
main = do
    input   <- (map read . words) <$> readFile "input_day6.txt"

    putStr "Part 1: "
    putStrLn $ show $ countRedistributions [] input

    putStr "Part 2: "
    putStrLn $ show $ countCycles [] input

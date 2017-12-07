import Data.List
import Data.Maybe
import Data.Either

parseLine :: String -> (String, (Int, [String]))
parseLine line          = (name, (weight, children))
    where
        parts           = words line
        name            = parts !! 0
        weight          = read $ takeWhile (/=')') $ tail $ parts !! 1
        children        = if length parts > 2
            then map (takeWhile (/=',')) $ drop 3 parts
            else []

findUnbalanced name input
    | length children == 0      = Left weight
    | hasUnbalancedChild        = Right $ head unbalancedChilds
    | childsMatch               = Left $ weight + (sum childSums)
    | otherwise                 = Right $ zip children childSums
    where
        (weight, children)      = fromJust $ lookup name input
        childResults            = map (flip findUnbalanced input) children
        childSums               = lefts childResults
        unbalancedChilds        = rights childResults
        hasUnbalancedChild      = length unbalancedChilds > 0
        childsMatch             = all (== (head childSums)) childSums

main = do
    input                       <- (map parseLine . lines) <$> readFile "input_day7.txt"

    let allNames                = map fst input
        allChildren             = concat $ map (snd . snd) input
        root                    = fromJust $ find (not . (`elem` allChildren)) allNames
    putStr "Part 1: "
    putStrLn $ show root

    let Right childs            = findUnbalanced root input
        first@(_, firstSum)     = head childs
        wrong@(name, wrongSum)  = fromMaybe first $ find ((/=firstSum) . snd) childs
        fittingSum              = if wrong == first
            then snd $ head $ tail childs
            else firstSum
        unfittingWeight         = fst $ fromJust $ lookup name input
        diff                    = unfittingWeight - (wrongSum - fittingSum)
    putStr "Part 2: "
    putStrLn $ show diff

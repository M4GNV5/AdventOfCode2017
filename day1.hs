import Data.List
import Data.Char

solveCaptcha list1 list2    = sum $ map (digitToInt . fst) $ filter (uncurry (==)) $ zip list1 list2

solveCaptcha1 str           = solveCaptcha str (tail str ++ [head str])

solveCaptcha2 str           = solveCaptcha str displacedStr
    where
        halfLen             = div (length str) 2
        displacedStr        = drop halfLen str ++ take halfLen str

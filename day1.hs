import Data.List
import Data.Maybe
import Data.Char

solveCaptcha d1 d2          = sum $ catMaybes $ zipWith matchesDigit d1 d2
    where
        matchesDigit x y    = if x == y
            then Just x
            else Nothing

solveCaptcha1 str           = solveCaptcha digits (tail digits)
    where
        digits              = map digitToInt (str ++ [head str])

solveCaptcha2 str           = solveCaptcha digits displacedDigits
    where
        digits              = map digitToInt str
        halfLen             = div (length digits) 2
        displacedDigits     = drop halfLen digits ++ take halfLen digits

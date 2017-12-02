import Control.Monad

checksum f                  = sum . map (f . (map read) . words) . lines

checksum1                   = checksum (liftM2 (-) maximum minimum)

checksum2                   = checksum firstEvenDivision 
    where
        isEvenDiv (x,y)     = y == 0 && x /= 1
        firstEvenDivision   = fst . head . filter isEvenDiv . join . (map =<< flip (map . divMod))

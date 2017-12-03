import Control.Monad

checksum f                  = sum . map (f . (map read) . words) . lines

checksum1                   = checksum (\x -> maximum x - minimum x)

checksum2                   = checksum firstEvenDivision
    where
        isEvenDiv (x, y)    = y == 0 && x /= 1
        firstEvenDivision x = fst $ head $ filter isEvenDiv $ concat $ map (\y -> map (divMod y) x) x

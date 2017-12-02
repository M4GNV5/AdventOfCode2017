checksum str f              = sum $ map (f . (map read) . words) (lines str)

checksum1 str               = checksum str (\x -> (maximum x) - (minimum x))

checksum2 str               = checksum str firstEvenDivision
    where
        isEvenDiv (x,y)     = y == 0 && x /= 1
        firstEvenDivision x = fst $ head $ filter isEvenDiv $ concat $ map (\y -> map (divMod y) x) x

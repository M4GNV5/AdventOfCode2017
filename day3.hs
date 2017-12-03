import Data.List
import Data.Ord

spiralCityBlock n   = distanceY + distanceX
    where
        num         = ceiling $ sqrt $ fromIntegral n
        rowNum      = num + 1 - (num `mod` 2)
        rowStart    = (rowNum - 2)^2 + 1
        row         = [rowStart .. (rowNum^2)]
        rowLength   = length row
        index       = n - rowStart
        centers     = [rowLength * i `div` 8 - 1 | i <- [1,3,5,7]]
        distanceX   = minimum $ map (abs . (index-)) centers
        distanceY   = rowNum `div` 2

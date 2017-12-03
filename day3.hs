import Data.List
import Data.Maybe
import Data.Ord

spiralCoordinates n = coords
    where
        num             = ceiling $ sqrt $ fromIntegral n
        rowNum          = num + 1 - (num `mod` 2)
        rowStart        = (rowNum - 2)^2 + 1
        row             = [rowStart .. (rowNum^2)]
        rowLength       = length row
        index           = n - rowStart
        centers         = [rowLength * i `div` 8 - 1 | i <- [1,3,5,7]]
        closestCenter   = minimumBy (comparing $ abs . (index-)) centers
        centerIndex     = fromJust $ elemIndex closestCenter centers
        distanceMidde   = rowNum `div` 2
        coords          = case centerIndex of
            0           -> (distanceMidde, index - closestCenter)
            1           -> (closestCenter - index, distanceMidde)
            2           -> (-distanceMidde, closestCenter - index)
            3           -> (index - closestCenter, -distanceMidde)

spiralCityBlock n       = abs x + abs y
    where
        (x, y)          = spiralCoordinates n

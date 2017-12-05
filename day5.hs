import Data.Vector

runInstruction :: Int -> Vector Int -> Int
runInstruction position tape
    | position < 0              = 0
    | position >= len           = 0
    | otherwise                 = 1 + runInstruction newPosition newTape
    where
        len                     = Data.Vector.length tape
        instruction             = tape ! position
        newPosition             = position + instruction
        newInstruction          = if instruction > 2
            then instruction - 1
            else instruction + 1
        newTape                 = tape // [(position, newInstruction)]

main = do
    tape                        <- fromList <$> Prelude.map read <$> lines <$> readFile "input_day5.txt"
    putStrLn $ show $ runInstruction 0 tape

data State = Content | Garbage | IgnoreC | IgnoreG deriving(Show)

parseStep (IgnoreC, level, score, count) _      = (Content, level, score, count)
parseStep (IgnoreG, level, score, count) _      = (Garbage, level, score, count)
parseStep (Content, level, score, count) '!'    = (IgnoreC, level, score, count)
parseStep (Garbage, level, score, count) '!'    = (IgnoreG, level, score, count)
parseStep (Content, level, score, count) '{'    = (Content, level + 1, score, count)
parseStep (Content, level, score, count) '}'    = (Content, level - 1, score + level, count)
parseStep (Content, level, score, count) '<'    = (Garbage, level, score, count)
parseStep (Garbage, level, score, count) '>'    = (Content, level, score, count)
parseStep (Garbage, level, score, count) _      = (Garbage, level, score, count + 1)
parseStep state _                               = state

main = do
    input                                       <- readFile "input_day9.txt"
    let (_, _, _, score, count)                 = foldl parseStep (Content, 0, 0, 0) input

    putStrLn $ "Part 1: " ++ (show score)
    putStrLn $ "Part 2: " ++ (show count)

data State = Content | Garbage | Ignore deriving(Show)

parseStep (Ignore, prev, level, score, count) _     = (prev, Ignore, level, score, count)
parseStep (prev, _, level, score, count) '!'        = (Ignore, prev, level, score, count)
parseStep (Content, _, level, score, count) '{'     = (Content, Ignore, level + 1, score, count)
parseStep (Content, _, level, score, count) '}'     = (Content, Ignore, level - 1, score + level, count)
parseStep (Content, _, level, score, count) '<'     = (Garbage, Ignore, level, score, count)
parseStep (Garbage, _, level, score, count) '>'     = (Content, Ignore, level, score, count)
parseStep (Garbage, _, level, score, count) _       = (Garbage, Ignore, level, score, count + 1)
parseStep state _                                   = state

main = do
    input                                           <- readFile "input_day9.txt"
    let (_, _, _, score, count)                     = foldl parseStep (Content, Ignore, 0, 0, 0) input

    putStrLn $ "Part 1: " ++ (show score)
    putStrLn $ "Part 2: " ++ (show count)

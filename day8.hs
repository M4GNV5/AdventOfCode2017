import Data.List
import Data.Maybe
import Data.Ord

comparasionOp "<" = (<)
comparasionOp ">" = (>)
comparasionOp "<=" = (<=)
comparasionOp ">=" = (>=)
comparasionOp "==" = (==)
comparasionOp "!=" = (/=)

modeOp "inc" = (+)
modeOp "dec" = (-)

runInstruction regs [reg, mode, val, _, condReg, cond, condVal]
    | condSuccess       = (reg, newVal) : filter ((/=reg) . fst) regs
    | otherwise         = regs
    where
        regVal          = fromMaybe 0 $ lookup reg regs
        condRegVal      = fromMaybe 0 $ lookup condReg regs
        condSuccess     = (comparasionOp cond) condRegVal (read condVal)
        newVal          = (modeOp mode) regVal (read val)

getMaxReg (oldMax,regs) strs    = (newMax, newRegs)
    where
        newRegs                 = runInstruction regs strs
        regValues               = map snd regs
        newMax                  = maximum $ oldMax : regValues

main = do
    input               <- (map words . lines) <$> readFile "input_day8.txt"

    putStr "Part 1:"
    putStrLn $ show $ maximumBy (comparing $ snd) $ foldl runInstruction [] input

    putStr "Part 2:"
    putStrLn $ show $ fst $ foldl getMaxReg (0, []) input

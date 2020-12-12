import qualified Data.Set as S

main = do
    prgm <- fmap (parseInst . words) . lines <$> readFile "./input8.txt" :: IO [Instr]
    putStr "Part 1: "
    print . snd $ simulate (length prgm) 0 prgm 0 S.empty
    let prgms = swap prgm <$> [0..length prgm]
    putStr "Part 2: "
    print . snd . maximum $ fmap (\p -> simulate (length p) 0 p 0 S.empty) prgms

data Instr = Acc Int
           | Jmp Int
           | Nop Int
    deriving (Show, Eq, Ord)

parseInst [inst, num] = let n = (read . filter (/= '+') $ num)
                        in case inst of
                             "acc" -> Acc n
                             "jmp" -> Jmp n
                             "nop" -> Nop n
                              
simulate limit state instrs pc history = if pc `S.member` history || pc >= limit
                                         then (pc,state)
                                         else simulate limit state' instrs pc' (pc `S.insert` history)
  where
    (state',pc') = case instrs !! pc of
                     Acc n -> (state + n, pc + 1)
                     Jmp n -> (state, pc + n)
                     Nop n -> (state, pc + 1)
    
swap instrs n = take n instrs ++ [inst] ++ drop (n+1) instrs
    where
      inst = case instrs !! n of
               Acc n -> Acc n
               Jmp n -> Nop n
               Nop n -> Jmp n

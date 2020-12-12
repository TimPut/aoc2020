import qualified Data.Set as S

main = do
    prgm <- fmap read . lines <$> readFile "./input9.txt" :: IO [Int]
    let p1 = (valid prgm 0)
    putStr "Part 1: "
    print p1
    putStr "Part 2: "
    print (xmas prgm p1)

valid ns m = if length ns <= 25
             then -1
             else rest
  where
    rest = let summands = take 25 ns
               m' = (ns !! 25)
           in if m' `notElem` [ x+y | x <- summands, y <- summands]
              then m'
              else valid (tail ns) m'

xmas ns n = let sums = dropWhile (\(a,b) -> a < n) . flip zip [0..] $ scanl1 (+) ns
            in if (fst . head $ sums) == n
               then let as = take (1 + (snd . head $ sums)) ns in (maximum as + minimum as)
               else xmas (tail ns) n
           

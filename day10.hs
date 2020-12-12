import qualified Data.Set as S
import Data.List

main = do
    prgm <- fmap read . lines <$> readFile "./input10.txt" :: IO [Int]
    print . (\(a,_,b) -> a*b) . count (0,0,1) . sort $ 0:prgm
    print (p2 prgm)
count (ons,tws,ths) (a:b:as) = case b-a of
                                 1 -> count (ons+1,tws,ths) (b:as)
                                 2 -> count (ons,tws+1,ths) (b:as)
                                 3 -> count (ons,tws,ths+1) (b:as)
                                 _ -> error "bad data"
count acc _ = acc

p2 ps = product
        . fmap combos
        . sort
        . filter (/= 1)
        . fmap length
        . filter (elem 1)
        . group
        $ zipWith (-) (tail ps') ps'
    where
      ps' = sort $ ((maximum ps) +3):0:ps
      combos 2 = 2
      combos 3 = 4
      combos 4 = 7
      combos _ = error "unseen case unimplemented"

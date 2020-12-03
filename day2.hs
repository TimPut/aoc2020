import qualified Data.ByteString.Char8 as B

main = do
    ps <- B.lines <$> B.readFile "./input2.txt" :: IO [B.ByteString]
    print . count valid $ ps
    print . count valid' $ ps

count _ [] = 0
count p (x:xs) = (if p x then 1 else 0) + count p xs

valid bs = let cnt = B.count c $ pass in cnt >= lb && cnt <= ub
  where
    [range, char, pass] = B.words bs
    [lb,ub] = case sequence . fmap (fmap fst) . fmap B.readInt $ B.splitWith (=='-') range
              of Nothing -> error "Can't parse bounds"
                 Just bounds -> bounds
    c = B.head char
    
valid' bs = let slotA = pass `B.index` (lb - 1) == c
                slotB = pass `B.index` (ub - 1) == c
            in case (slotA,slotB) of
                 (True,False) -> True
                 (False,True) -> True
                 _ -> False
  where
    [range, char, pass] = B.words bs
    [lb,ub] = case sequence . fmap (fmap fst) . fmap B.readInt $ B.splitWith (=='-') range
              of Nothing -> error "Can't parse bounds"
                 Just bounds -> bounds
    c = B.head char

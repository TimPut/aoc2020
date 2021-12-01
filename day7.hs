{-# LANGUAGE OverloadedStrings #-}

import Debug.Trace
import Data.List
import qualified Data.HashMap.Strict as M
import qualified Data.ByteString.Char8 as B
import qualified Data.Set as S
main = do
    rs <- B.lines <$> B.getContents
    let m = M.fromListWith (<>) . concat $ fmap clean rs
        m' = M.fromListWith (<>) $ fmap clean' rs
    -- print $ transitive m ["shiny gold"] S.empty
    -- print $ (S.size $ transitive m ["shiny gold"] S.empty) - 1
    print  $ bags m' "shiny gold"
transitive :: M.HashMap B.ByteString [B.ByteString] -> [B.ByteString] -> S.Set B.ByteString -> S.Set B.ByteString
transitive _ [] _ = S.empty
transitive m frontier seen = let frontier' = nub . filter (flip S.notMember seen) . concat $ (fmap find frontier)
                                 seen' = S.fromList frontier
                             in seen' <> transitive m frontier' (seen')
    where
      find r = M.findWithDefault [] r m
      nub = S.toList . S.fromList

clean bs = [(t,[s']) | t <- ts']
    where
      (s,ts) = B.breakSubstring " contain " bs
      ts' = fmap B.unwords
          . fmap tail
          . fmap (filter (\w -> w /= "bags" && w /= "bag"))
          . fmap B.words
          . B.split ','
          . B.filter (/= '.')
          . B.drop 9 $ ts
      s' = B.unwords . (filter (\w -> w /= "bags" && w /= "bag"))
          . B.words $ s


bags m r = (sum . fmap (\(f,b) -> 1 + f * (bags m b)) $ find r)
    where
      find r = M.findWithDefault [] r m


-- clean' :: B.ByteString -> (B.ByteString,[(Int,B.ByteString)])
clean' bs = (s',ts')
    where
      (s,ts) = B.breakSubstring " contain " bs
      ts' = fmap (\xs -> if "no" `elem` xs then (1,"") else (read . B.unpack . head $ xs, B.unwords . tail $ xs))
          . fmap (filter (\w -> w /= "bags" && w /= "bag"))
          . fmap B.words
          . B.split ','
          . B.filter (/= '.')
          . B.drop 9 $ ts
      s' = B.unwords . (filter (\w -> w /= "bags" && w /= "bag"))
          . B.words $ s


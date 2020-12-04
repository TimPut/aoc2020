{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}


import qualified Data.ByteString.Char8 as B
import Data.ByteString.Char8 (ByteString)
import Data.List (sort)
import qualified Data.HashMap as M
import Data.HashMap (Map)
import Data.Char

main = do
    rs <- fmap (fmap (\bs -> " " `B.append` bs)) . sep . B.lines <$> B.getContents :: IO [[ByteString]]
    print . count id . fmap validate . fmap B.concat $ rs
    print . count id . fmap validate2 . filter (validate) . fmap B.concat $ rs

sep :: [ByteString] -> [[ByteString]]
sep [] = []
sep rs = (\(a,b) -> a:sep (if b == [] then b else tail b)) $ span (not . B.null) rs

validate r = let fs = (sort . fmap head . fmap (B.split ':') $ B.words r) in fs == fields || fs == fields'

validate2 r = let record :: Map ByteString ByteString = M.fromList . fmap (\[a,b] -> (a, b)) . fmap (B.split ':') . B.words $ r
                  rl k = record M.! k
              in and [inRange (digits $ rl "byr") 1920 2002
                     , inRange (digits $ rl "iyr") 2010 2020
                     , inRange (digits $ rl "eyr") 2020 2030
                     , let bs = rl "hgt" in if B.last bs == 'n' then inRange (digits bs) 59 76 else inRange (digits bs) 150 193
                     , let bs = rl "hcl" in B.head bs == '#' && all (\e -> elem e ("0123456789abcdef" :: String)) (B.unpack . B.tail $ bs)
                     , let bs = rl "ecl" in elem bs colors
                     , let bs = rl "pid" in B.length bs == 9 && bs == B.filter isDigit bs
                     ]

colors = ["amb","blu","brn","gry","grn","hzl","oth"]

digits = read . B.unpack . B.filter isDigit

inRange :: Int -> Int -> Int -> Bool
inRange v l u = l <= v && v <= u

fields :: [ByteString]
fields = sort ["byr","iyr","eyr","hgt","hcl","ecl","pid"]
fields' = sort ["byr","iyr","eyr","hgt","hcl","ecl","pid","cid"]

count _ [] = 0
count p (x:xs) = (if p x then 1 else 0) + count p xs

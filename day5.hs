import           Data.Bits
import           Data.List
import           Data.Word

main :: IO ()
main = do
    ls <- sort . fmap bs2w . fmap (fmap (\c -> c `elem` "BR")) . lines <$> readFile "./input5.txt"
    putStr "Part 1:"
    print . head $ ls
    putStr "Part 2:"
    print . (\w -> w-1) . snd . head . dropWhile (\x -> fst x == head ls) $ zip (zipWith (-) ls [0,1..]) ls

type BitString = [Bool]

bs2w :: BitString -> Word32
bs2w = go 0
  where
    go acc []     = acc
    go acc (True:bs) = go (shiftL acc 1 .|. 1) bs
    go acc (False:bs) = go (shiftL acc 1) bs

w2bs :: Word32 -> BitString
w2bs w = fmap (\i -> (bit i .&. w) /= 0) [31,30..0]


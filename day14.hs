import           Data.Bits
import           Data.List
import           Data.Word
import Data.Char
import qualified Data.IntMap as M
import Debug.Trace


main = do
    ls <- lines <$> readFile "./input14.txt" :: IO [String]
    print $ simulate ls
    print ()
mkOrMask :: String -> Word
mkOrMask str = readBin $ fmap (\c -> if c == '1' then '1' else '0') str

mkAndMask :: String -> Word
mkAndMask str = readBin $ fmap (\c -> if c == '0' then '0' else '1') str

data ParseResult = MemSet Int Word
                 | Mask Word Word
  deriving (Show, Eq, Ord)
parseLine l = case code of
                ('m':'e':'m':cs) -> MemSet (read (filter (isDigit) cs))
                                   (read n)
                _ -> Mask (mkAndMask n) (mkOrMask n)
  where
    [code,_,n] = words l

simulate ls = go (Mask 0 0) M.empty ls
  where
    go _ m [] = sum . M.elems $ m
    go (Mask a o) m (l:ls) = case parseLine l of
                               MemSet i w -> go (Mask a o) (M.insert i ((w .&. a) .|. o) m) ls
                               Mask a' o' -> go (Mask a' o') m ls

readBin str = go 0 str
    where
      go acc [] = acc
      go acc ('1':bs) = go (2*acc+1) bs
      go acc ('0':bs) = go (2*acc) bs

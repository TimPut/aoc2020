import qualified Data.Set as S
import Data.List

main = do
    bs <- readFile "./input6.txt" :: IO String
    let unio = sum . fmap (S.size . S.fromList) . fmap (filter (/= '\n')) . doubleLines $ bs
    putStr "Part 1: "
    print unio
    let intr = sum . fmap (S.size . foldr1 S.intersection . fmap S.fromList . lines) . doubleLines $ bs
    putStr "Part 2: "
    print intr

doubleLines :: String -> [String]
doubleLines str = go [] [] str
    where
      go acc x [] = reverse $ fmap reverse $ x:acc
      go acc x ('\n':'\n':as) = go (x:acc) [] as
      go acc x (a:as) = go acc (a:x) (as)

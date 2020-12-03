import qualified Data.ByteString.Char8 as B
import Data.ByteString.Char8 (ByteString)
import qualified Data.Vector as V
import Data.Vector (Vector)

main = do
    rs <- V.fromList . B.lines <$> B.getContents :: IO (Vector ByteString)
    print . part1 $ rs
    print . product . fmap (uncurry (part2 rs)) $ [(1,1),(3,1),(5,1),(7,1),(1,2)]

part1 rs = part2 rs 3 1

part2 :: Vector ByteString -> Int -> Int -> Int
part2 rs dx dy = go 0 (0,0)
    where
      n = B.length . V.head $ rs
      go acc (x,y) = if y >= V.length rs
                     then acc
                     else go (if ((rs V.! y) `B.index` x) == '#'
                              then acc+1
                              else acc) ((x+dx) `mod` n, y+dy)


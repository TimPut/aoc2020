import Data.Maybe
import qualified Data.Vector as V

show' H = "#"
show' L = "L"
show' O = "."

main = do
    ls <- lines <$> readFile "./input11.txt" :: IO [String]
    let n = length . head $ ls
        m = length ls
    let plan = V.fromList . concat . fmap (fmap readSpot) $ ls
        fixPoint = fix (solve n m) plan
    putStr . unlines
        . fmap (concat . fmap show')
        . chunksOf n . V.toList $ fixPoint
    print . count . V.toList $ fixPoint
     
chunksOf :: Int -> [a] -> [[a]]
chunksOf i xs | i <= 0 = error $ "chunksOf, number must be positive, got " ++ show i
chunksOf i xs = repeatedly (splitAt i) xs         

repeatedly :: ([a] -> (b, [a])) -> [a] -> [b]
repeatedly f [] = []
repeatedly f as = b : repeatedly f as'
    where (b, as') = f as

data Spot = L
          | H
          | O
  deriving (Show, Eq)

ixToXY n i = (i `mod` n, i `div` n)
xyToIx n (x,y) = y*n+x

readSpot '.' = O
readSpot '#' = H
readSpot 'L' = L

transition L n = if n == 0 then H else L
transition H n = if n >= 5 then L else H
transition O _ = O

solve n m = \p -> (V.imap (\i a -> let c = neighbors n m p (ixToXY n i) in transition a c)) p
                                          
neighbors :: Int -> Int -> V.Vector Spot -> (Int,Int) -> Int
neighbors n m plan (x,y) = let offsets = [ xyToIx n (x+dx,y+dy)
                                         | dx<-[-1,0,1]
                                         , dy<-[-1,0,1]
                                         -- , not(dx==0 && dy==0)
                                         , (x+dx) >= 0
                                         , (x+dx) < n
                                         , (y+dy) >= 0
                                         , (y+dy) < m
                                         ]
                           in count $ fmap (\i -> fromMaybe O (plan V.!? i)) offsets

count = length . filter (== H)

fix f x | f x == f (f x)  = f x
        | otherwise       = fix f (f x)

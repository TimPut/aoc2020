import Debug.Trace
import Data.List (splitAt, elemIndex)


sample, input :: [Int]
input = [4,5,9,6,7,2,8,1,3]
sample = [3,8,9,1,2,5,4,6,7]
main = do
    
    pure ()

step n (x:a:b:c:xs) = rotate 1
                      $ (\(y,z) -> y ++ (a:b:[c]) ++ z)
                      $ splitAt (destination x xs)
                      $ (x:xs)
  where
    destination x xs = case elemIndex ((x-1)`mod`n) (x:xs) of
                                       Nothing -> destination ((x-1)`mod`n) xs
                                       Just i -> i+1
rotate1 = rotate 1

rotate :: Int -> [a] -> [a]
rotate _ [] = []
rotate n xs = zipWith const (drop n (cycle xs)) xs

solve n l xs = take 8
             . tail
             . dropWhile (/= 1)
             . cycle $ (iterate (step l) xs) !! n

solve' n xs = solve 10000000 1000000 (xs ++ [10..1000000])

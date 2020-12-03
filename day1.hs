main = do
    ls <- fmap read . lines <$> readFile "./input1.txt" :: IO [Int]
    putStr "Part 1:"
    print . head $ [ x * y | x <- ls, y <- ls, x + y == 2020]
    putStr "Part 2:"
    print . head $ [ x * y * z | x <- ls, y <- ls, z <- ls, x + y + z == 2020]

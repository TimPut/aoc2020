main = do
    file <- lines <$> readFile "./input13.txt"
    let t0 = read . head $ file :: Int
        bs = fmap (read :: String -> Int)
             . filter (/= "x")
             . split ',' . head . tail $ file
    print t0
    print . length . head . tail $ file
    print bs
    print . (\(a,b) -> a*b) . minimum . fmap (\(r,b) -> (b-r,b))
        $ zip (fmap (\b -> t0 `mod` b) bs) bs
split :: Char -> String -> [String]
split c s = case rest of
                []     -> [chunk]
                _:rest -> chunk : split c rest
  where (chunk, rest) = break (==c) s

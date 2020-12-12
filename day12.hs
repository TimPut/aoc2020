import qualified Data.Set as S

main = do
    prgm <- fmap parseMove . lines <$> readFile "./input12.txt" :: IO [Move]
    putStr "Part 1: "
    print . (\(a,b) -> abs a + abs b) $ simulate (0,0) E' prgm
    putStr "Part 2: "
    print . (\(a,b) -> abs a + abs b) $ simulate2 (0,0) (10,1) prgm

data Move = N Int
          | S Int
          | E Int
          | W Int
          | L Int
          | R Int
          | F Int
    deriving (Show, Eq, Ord)

-- Orientations are distinct from directed translations
data Dir = N'
         | S'
         | E'
         | W'
    deriving (Show, Eq, Ord, Enum)

parseMove l = let n = (read . tail $ l)
              in case head l of
                   'N' -> N n
                   'S' -> S n
                   'E' -> E n
                   'W' -> W n
                   'L' -> L n
                   'R' -> R n
                   'F' -> F n

simulate :: (Int,Int) -> Dir -> [Move] -> (Int,Int)
simulate (x,y) _ [] = (x,y)
simulate (x,y) dir (m:mvs) = case m of
                             N n -> simulate (x,y+n) dir mvs
                             S n -> simulate (x,y-n) dir mvs
                             E n -> simulate (x+n,y) dir mvs
                             W n -> simulate (x-n,y) dir mvs
                             L n -> simulate (x,y) (turn n dir) mvs
                             R n -> simulate (x,y) (turn (negate n) dir) mvs
                             F n -> case dir of
                                     N' -> simulate (x,y+n) dir mvs
                                     S' -> simulate (x,y-n) dir mvs
                                     E' -> simulate (x+n,y) dir mvs
                                     W' -> simulate (x-n,y) dir mvs
    where
      turn n d = toEnum $ (fromEnum d + ((n `div` 90) `mod` 4)) `mod` 4

simulate2 :: (Int,Int) -> (Int,Int) -> [Move] -> (Int,Int)
simulate2 (u,v) (x,y) [] = (u,v)
simulate2 (u,v) (x,y) (m:mvs) = case m of
                             N n -> simulate2 (u,v) (x,y+n) mvs
                             S n -> simulate2 (u,v) (x,y-n) mvs
                             E n -> simulate2 (u,v) (x+n,y) mvs
                             W n -> simulate2 (u,v) (x-n,y) mvs
                             L 90 -> simulate2 (u,v) (-y,x) mvs
                             R 90 -> simulate2 (u,v) (y,-x) mvs
                             L 180 -> simulate2 (u,v) (-x,-y) mvs
                             R 180 -> simulate2 (u,v) (-x,-y) mvs
                             L 270 -> simulate2 (u,v) (y,-x) mvs
                             R 270 -> simulate2 (u,v) (-y,x) mvs
                             F n -> simulate2 (u+n*x,v+n*y) (x,y) mvs

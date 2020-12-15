{-# LANGUAGE BangPatterns #-}

import qualified Data.Vector.Unboxed.Mutable as V
import qualified Data.IntMap.Strict as M

input = [11,0,1,10,5,19]

main = do
    print =<< solve2 input 2020
    print =<< solve2 input 30000000
    -- let vist = M.fromList $ zip input [0..]
    -- print $ solve (hist,(length input)-1,last input, 2020)
    -- print $ solve (hist,(length input)-1,last input, 30000000)

-- map, program counter, last value, stopping point
solve (m,pc,lv,sp) = go m pc lv
  where
    go !m !pc !lv = let lv' = pc - M.findWithDefault pc lv m
                        pc' = pc + 1
                        m' = M.insert lv pc m
                    in if pc' == sp
                       then lv
                       else go m' pc' lv'

solve2 xs sp = do
    v <- V.replicate sp (-1)
    sequence_ $ fmap (\(i,j) -> V.unsafeWrite v i j) (zip xs [0..])
    go v ((length xs) -1) (last xs)
  where
    go !v !pc !lv = do
        op <- V.unsafeRead v lv
        let lv' = if op == -1 then 0 else pc - op 
            pc' = pc + 1
        V.unsafeWrite v lv pc
        if pc' == sp then pure lv else go v pc' lv'

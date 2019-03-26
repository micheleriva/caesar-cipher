module Lib
    ( caesar
    ) where

import Data.Char (toUpper)

filterStr :: String -> String
filterStr [] = []
filterStr xs = filter (\x -> x `elem` ['A'..'Z']) $ map toUpper xs

shiftForward :: Char -> Char
shiftForward 'Z' = 'A'
shiftForward  c  = succ c

shiftBackward :: Char -> Char
shiftBackward 'A' = 'Z'
shiftBackward  c  = pred c

caesar :: String -> Int -> String
caesar str n
    | n > 0  = caesar (map shiftForward  s) $ n - 1
    | n < 0  = caesar (map shiftBackward s) $ n + 1
    | n == 0 = s
    where s = filterStr str
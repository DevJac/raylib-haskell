module Utils where
import Data.Bits
import Data.List

combineBitflags :: (Enum a, Num b, Bits b) => [a] -> b
combineBitflags = foldl' (\x y -> x .|. fromIntegral (fromEnum y)) 0

module Internal.Utils where
import Data.Bits (Bits((.|.)))

combineBitflags :: (Enum a, Num b, Data.Bits.Bits b) => [a] -> b
combineBitflags = foldr (\a b -> fromIntegral (fromEnum a) .|. b) 0

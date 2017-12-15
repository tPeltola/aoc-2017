module Fifteen where

import Data.Bits

maxValue = 2147483647

testA = a 65
testB = b 8921

inputA = a 722
inputB = b 354

generateA previous = (previous * 16807) `rem` maxValue
generateB previous = (previous * 48271) `rem` maxValue

a first = tail $ iterate generateA first
b first = tail $ iterate generateB first

match a b = (0x0FFFF .&. a) == (0x0FFFF .&. b)

matches a b pairs = length $ filter id $ take pairs $ zipWith match a b

pickyA = filter ((== 0) . (`mod` 4))
pickyB = filter ((== 0) . (`mod` 8))
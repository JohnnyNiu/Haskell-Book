module Idempotence where

import Test.QuickCheck
import Data.List
import Data.Char

twice f = f . f

fourTimes = twice . twice

-- exercise one
capitalizeWord = concat
           . map (\(c:cs) -> toUpper c : cs)
           . groupBy (\a b -> isSpace a == isSpace b)

prop_capital :: String -> Bool
prop_capital x = (capitalizeWord x == twice capitalizeWord x)
        && (capitalizeWord x == fourTimes capitalizeWord x)

capitalQc :: IO ()
capitalQc = quickCheck prop_capital

-- exercise two

prop_sort :: [Int] -> Bool
prop_sort x = (sort x  == twice sort x) && (sort x == fourTimes sort x)

sortQc :: IO ()
sortQc = quickCheck prop_sort
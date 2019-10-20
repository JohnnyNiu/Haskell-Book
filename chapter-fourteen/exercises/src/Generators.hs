module Generators where

-- exercise one

data Fool = Fulse | Frue deriving (Eq, Show)

genFool :: Gen Fool
genFool = choose (Fulse, Frue)

genFool' :: Gen Fool
genFool' = frequency [(2, Fulse),(1, Frue)]
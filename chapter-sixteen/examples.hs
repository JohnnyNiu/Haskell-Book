module Examples where

-- data FixMePls =
--     FixMe | Pls
--     deriving (Eq, Show)

data FixMePls a =
    FixMe | Pls a
    deriving (Eq, Show)

-- instance Functor FixMePls where
--     fmap =
--         error
--         "it doesn't matter, it won't compile"

instance Functor FixMePls where
    fmap _ FixMe = FixMe
    fmap f (Pls a) = Pls (f a)

replaceWithP :: b -> Char
replaceWithP = const 'p'

lms :: [Maybe [Char]]
lms = [Just "Ave", Nothing, Just "woohoo"]

-- Just making the argument more specific
replaceWithP' :: [Maybe [Char]] -> Char
replaceWithP' = replaceWithP

-- lifting it
-- Prelude> :t fmap replaceWithP
-- fmap replaceWithP :: Functor f
-- => f a -> f Char

liftedReplace :: Functor f => f a -> f Char
liftedReplace = fmap replaceWithP

liftedReplace' :: [Maybe [Char]] -> [Char]
liftedReplace' = liftedReplace

-- lifting it again

-- Prelude> :t (fmap . fmap) replaceWithP
-- (fmap . fmap) replaceWithP
-- :: (Functor f1, Functor f)
-- => f (f1 a) -> f (f1 Char)
twiceLifted :: (Functor f1, Functor f) =>
    f (f1 a) -> f (f1 Char)
twiceLifted = (fmap . fmap) replaceWithP

-- Making it more specific
twiceLifted' :: [Maybe [Char]] -> [Maybe Char]
twiceLifted' = twiceLifted
-- f ~ []
-- f1 ~ Maybe

-- Prelude> let rWP = replaceWithP
-- Prelude> :t (fmap . fmap . fmap) rWP
-- (fmap . fmap . fmap) replaceWithP
-- :: (Functor f2, Functor f1, Functor f)
-- => f (f1 (f2 a)) -> f (f1 (f2 Char))
thriceLifted ::
    (Functor f2, Functor f1, Functor f)
    => f (f1 (f2 a)) -> f (f1 (f2 Char))
thriceLifted = (fmap . fmap . fmap) replaceWithP

-- More specific or "concrete"
thriceLifted' :: [Maybe [Char]] -> [Maybe [Char]]
thriceLifted' = thriceLifted
-- f ~ []
-- f1 ~ Maybe
-- f2 ~ []

main :: IO ()
main = do
    putStr "replaceWithP' lms: "
    print (replaceWithP' lms)
    putStr "liftedReplace lms: "
    print (liftedReplace lms)
    putStr "liftedReplace' lms: "
    print (liftedReplace' lms)
    putStr "twiceLifted lms: "
    print (twiceLifted lms)
    putStr "twiceLifted' lms: "
    print (twiceLifted' lms)
    putStr "thriceLifted lms: "
    print (thriceLifted lms)
    putStr "thriceLifted' lms: "
    print (thriceLifted' lms)
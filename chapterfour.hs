module ChapterFour where

data Mood = Blah | Woot deriving Show

changeMood :: Mood -> Mood
changeMood Blah = Woot
changeMood    _ = Blah

isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome x = if (x == reverse x)
                    then True
                    else False

myAbs :: Integer -> Integer
myAbs x = if (x < 0)
            then x * (-1)
            else x
        
f :: (a, b) -> (c, d) -> ((b, d), (a, c))
f x y = ((snd x, snd y), (fst x, fst y))
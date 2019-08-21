module ChapterThree where

addExclamation :: String -> String
addExclamation x = x ++ "!"

fourthLetter :: String -> Char
fourthLetter x = x !! 4

dropNine :: String -> String
dropNine x = drop 9 x

thirdLetter :: String -> Char
thirdLetter x = x !! 2

letterIndex :: Int -> Char
letterIndex x = "Curry is awesome!" !! x

rvrs :: String -> String
rvrs n      = x ++ (y ++ z)
    where x = dropNine n
          y = take 4 m
          m = drop 5 n
          z = take 5 n

main :: IO ()
main = print $ rvrs "Curry is awesome"
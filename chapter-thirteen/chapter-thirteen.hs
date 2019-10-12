module ChapterThirteen where
    
import Control.Monad
import System.Exit (exitSuccess)
import Data.Char
import System.IO

    
palindrome :: IO ()
palindrome = forever $ do
    line1 <- getLine
    case (line1 == reverse line1) of
        True -> putStrLn "It's a palindrome!"
        False -> do putStrLn "Nope!"
                    exitSuccess

removePunc xs = [ x | x <- xs, not (x `elem` ",.?!-:;\"\' ") ]

palindrome' :: IO ()
palindrome' = forever $ do
    line1 <- getLine
    let line = map toLower (removePunc line1)
    case (line == reverse line) of
        True -> putStrLn "It's a palindrome!"
        False -> do putStrLn "Nope!"
                    exitSuccess


type Name = String

type Age = Integer

data Person = Person Name Age deriving Show

data PersonInvalid =
    NameEmpty
    | AgeTooLow
    | PersonInvalidUnknown String
    deriving (Eq, Show)

mkPerson :: Name -> Age -> Either PersonInvalid Person
mkPerson name age
    | name /= "" && age > 0 = Right $ Person name age
    | name == "" = Left NameEmpty
    | not (age > 0) = Left AgeTooLow
    | otherwise =
        Left $ PersonInvalidUnknown $
        "Name was: " ++ show name ++
        " Age was: " ++ show age

gimmePerson :: IO ()
gimmePerson = do
    hSetBuffering stdout NoBuffering
    putStrLn "Enter a name: "
    name <- getLine
    putStrLn "Enter an age: "
    ageInput <- getLine
    let age = read ageInput::Integer
    let person = mkPerson name age
    case person of
        Right p -> putStrLn $ "Yay! Successfully got a person: " ++ show p
        Left e -> putStrLn $ "Sorry. An error occurred: " ++ show e 


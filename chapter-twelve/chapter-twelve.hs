module ChapterTwelve where

-- string processing

notThe :: String -> Maybe String
notThe str  
    | str == "the" = Nothing
    | otherwise = Just str

replaceThe :: String -> String
replaceThe str = foldr (notThe)


module ChapterTen where

    
filterDbDate :: [DatabaseItem] -> [UTCTime]
filterDbDate db = foldr f [] db
        where
            f (DbDate x) acc = x : acc
            f _ acc = acc

filterDbNumber :: [DatabaseItem] -> [Integer]
filterDbNumber = foldr f [] db
        where
            f (DbNumber x) acc = x : acc
            f _ acc = acc

mostRecent :: [DatabaseItem] -> UTCTime
mostRecent = foldr f (UTCTime 0) db
        where
            f (DbDate x) acc = if x > acc then acc = x
            f _ acc = acc

sumDb :: [DatabaseItem] -> Integer
sumDb = foldr f 0 db
        where
            f (DbNumber x) acc = x + acc
            f _ acc = acc

avgDb :: [DatabaseItem] -> Double
avgDb = foldr f 0 db
        where
            f (DbNumber x) acc = x + acc
            f _ acc = (fromIntegral acc) / (fromIntegral (length theDatabase))

            
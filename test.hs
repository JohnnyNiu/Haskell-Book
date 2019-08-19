sayHello :: String -> IO ()
sayHello x = putStrLn ("hello, " ++ x ++ "!")

triple x = x * 3
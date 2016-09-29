sayHello :: String -> IO ()
sayHello x = putStrLn ("hello, " ++ x ++ "!")

triple x = x*3

multi = z/x + y
  where x = 7
        y = negate x
        z = y * 10

waxOn = x*5
    where
        z = 7
        x = y^2
        y = z+8


waxOff x = triple x

third :: [Char] -> Char
third x = last (take 3 x)

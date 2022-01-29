module Print3 where


myGreetings = "hello " ++ " world!"

hello = "Hello"

world = "world"

main :: IO ()
main = do
        putStrLn helloWorld
        where
          helloWorld = concat [hello, " " , world]



module BasicMonads (basicMonadsMain1, basicMonadsMain2, basicMonadsMain3) where


askUser :: IO [Integer]
askUser = do
    putStrLn "Enter a list of numbers (separated by comma):"
    input <- getLine
    case reads $ "[" ++ input ++ "]" of
        [(list, "")] -> return list
        _            -> askUser


-- "hand made" `bind`
bind :: IO a -> (a -> IO b) -> IO b
bind action1 action2 = do
    x <- action1
    action2 x


-- "hand made" `blindBind`
blindBind :: IO a -> IO b -> IO b
blindBind action1 action2 =
    bind action1 (\_ -> action2)


-- using "do" notation
basicMonadsMain1 :: IO ()
basicMonadsMain1 = do
    putStrLn "In basicMonadsMain1..."
    list <- askUser
    print $ (sum list) + 1
    putStrLn "That was fun..."


-- using Haskell's ">>=" ("bind" or "shove") and ">>" ("blind bind")
basicMonadsMain2 :: IO ()
basicMonadsMain2 =
    putStrLn "In basicMonadsMain2..." >>
    askUser >>= (\list ->
    print $ (sum list) + 2) >>
    putStrLn "That was fun..."


-- using "hand made" `bind` and `blindBind`
basicMonadsMain3 :: IO ()
basicMonadsMain3 =
    putStrLn "In basicMonadsMain3..." >>
    askUser `bind` (\list ->
    print $ (sum list) + 3) `blindBind`
    (putStrLn "That was fun...")

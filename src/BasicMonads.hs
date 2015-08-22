import Data.Maybe


maybeRead :: Read a => String -> Maybe a
maybeRead s = case reads s of
                  [(x,"")]    -> Just x
                  _           -> Nothing


getListFromString :: String -> Maybe [Integer]
getListFromString str = maybeRead $ "[" ++ str ++ "]"


askUser :: IO [Integer]
askUser = do
  putStrLn "Enter a list of numbers (separated by comma):"
  input <- getLine
  let maybeList = getListFromString input in
      case maybeList of
          Just l  -> return l
          Nothing -> askUser


bind :: IO a -> (a -> IO b) -> IO b
bind action1 action2 = do
    x <- action1
    action2 x


blindBind :: IO a -> IO b -> IO b
blindBind action1 action2 =
    bind action1 (\_ -> action2)


main :: IO ()
main =
  (askUser `bind` (\list ->
  (print . (2+) . sum) list)) `blindBind`
  (putStrLn "That was fun...")

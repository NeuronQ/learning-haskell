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


main :: IO ()
main =
  askUser `bind`
  (print . (1+) . sum)

module Helpers where

import Debug.Trace
import Data.Char (isSpace)

end :: String -> IO a
end e = ioError(userError e)

parseToString :: Char -> String
parseToString char = [char]

trim :: [Char] -> [Char]
trim = f . f
   where f = reverse . dropWhile isSpace

parseToRulesArray :: [[a]] -> [a]
parseToRulesArray rule = concat rule

debug = flip trace


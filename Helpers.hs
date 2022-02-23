module Helpers where

import Debug.Trace
import Data.Char (isSpace)

-- This function ends the code with appropriate error message to the stdout
end :: String -> IO a
end e = ioError(userError e)

-- Simple parser that changes Char to String
parseToString :: Char -> String
parseToString char = [char]

-- Function that trims leading and trailing whitespace
trim :: [Char] -> [Char]
trim = f . f
   where f = reverse . dropWhile isSpace

-- Change array of arrays to simple array
parseToRulesArray :: [[a]] -> [a]
parseToRulesArray rule = concat rule

-- Print out debug string
debug = flip trace


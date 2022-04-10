-- Project: plg-2-nka
-- Author: Jakub Smejkal (xsmejk28)
-- Year: 2022

-- Module containing some simple helper functions
module Helpers where

import Data.Char (isSpace)

-- This function ends the code with appropriate error message to the stdout
endWithError :: String -> IO a
endWithError errorText = ioError(userError errorText)

-- Simple parser that changes Char to String
charToString :: Char -> String
charToString char = [char]

-- Function that trims leading and trailing whitespace
trim :: [Char] -> [Char]
trim = (reverse . dropWhile isSpace) . (reverse . dropWhile isSpace)

-- Change array of arrays to simple array
parseToRulesArray :: [[a]] -> [a]
parseToRulesArray rule = concat rule
-- Project: plg-2-nka
-- Author: Jakub Smejkal (xsmejk28)
-- Year: 2022

-- Main module containing parameter parsing, file reading, etc.

import Types
import Helpers (endWithError)
import NKAProc
import PlgProc (getUpdatedPlg)
import PlgParser
import System.Environment (getArgs)

-- Main function, checks the input and print the desired output or error
main :: IO ()
main = do
    (scriptArgument, input) <- processArguments =<< getArgs
    either endWithError scriptArgument (createAndCheckGrammar input)

-- Functions that gets input from selected file or from the stdin
getInput :: String -> IO String
getInput "" = getContents
getInput filename = readFile filename

-- Function that parses command line arguments 
parseArguments :: [String] -> IO(String, String)
parseArguments [scriptArgument] = return (scriptArgument, "")
parseArguments [scriptArgument, filename] = return (scriptArgument, filename)
parseArguments _ = endWithError "Error in arguments.\nPlease provide on of the argument and optionally name of the file with inputs\n -i - print the loaded grammar\n -1 - print updated grammar\n -2 - print the NKA created from the input grammar"

-- Function that processes arguments and returns specific function for each argument to get the desired output
processArguments :: [String] -> IO(PlgGrammar -> IO(), String)
processArguments args = do
    (scriptArgument, filename) <- parseArguments args
    input <- getInput filename
    let inputLines = (length (lines input))
    if inputLines >= 4
      then do
        case scriptArgument of
          "-i" -> return(returnPlg, input)
          "-1" -> return(returnUpdatedPlg, input)
          "-2" -> return(returnNka, input)
          _    -> endWithError ("Not a valid argument. Please use -i/-1/-2")
    else endWithError "Input file must have 4 lines or more (Nonterminals, Terminals, Start Nonterminal and at least one Rule)"

-- Functions that just prints the same grammar that was given as the input 
returnPlg :: PlgGrammar -> IO()
returnPlg plg = do
  print plg

-- Function that prints out the updated grammar that is ready to be transfered to NKA
returnUpdatedPlg :: PlgGrammar -> IO()
returnUpdatedPlg plg = do
  print $ getUpdatedPlg plg

-- Function that prints out equivalent NKA for the grammar that was given as the input
returnNka :: PlgGrammar -> IO()
returnNka plg = do
  print $ getFinalNka $ getUpdatedPlg plg
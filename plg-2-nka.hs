import Helpers
import Data.Char
import System.IO (isEOF)
import PlgProc
import Types
import PlgParser
import NKAProc
import System.Environment ( getArgs )

-- Main function, checks the input and print the desired output or error
main :: IO ()
main = do
    (flag, input) <- processArguments =<< getArgs
    
    either end flag (createAndCheckGrammar input)

-- Functions that gets input from selected file or from the stdin
getInput :: String -> IO String
getInput "" = getContents
getInput filename = readFile filename

-- Function that parses command line arguments 
parseArguments :: [String] -> IO(String, String)
parseArguments [flag] =             return (flag, "")
parseArguments [flag, filename] =   return (flag, filename)
parseArguments others =             end "Error in arguments. Expecting arguments in format -i|-1|-2 [FILE]"

-- Function that processes arguments and returns specific function for each flag to get the desired output
processArguments :: [String] -> IO(PlgGrammar -> IO(), String)
processArguments args = do
    (flag, filename) <- parseArguments args
    input <- getInput filename
    let inputLines = (length (lines input))
    if inputLines >= 4
      then do
        case flag of
          "-i" -> return(returnPlg, input)
          "-1" -> return(returnUpdatedPlg, input)
          "-2" -> return(returnNka, input)
          _    -> end ("Not a valid flag. Please use -i|-1|-2")
    else end "Input file must have 4 lines or more (Nonterminals, Terminals, Start Nonterminal and Rules)"

-- Functions that just prints the same grammar that was given as the input 
returnPlg :: PlgGrammar -> IO()
returnPlg plg = do
  putStrLn "Printing base PLG"
  print plg

-- Function that prints out the updated grammar that is ready to be transfered to NKA
returnUpdatedPlg :: PlgGrammar -> IO()
returnUpdatedPlg plg = do
  putStrLn "Printing updated PLG"
  print $ getUpdatedPlg plg

-- Function that prints out equivalent NKA for the grammar that was given as the input
returnNka :: PlgGrammar -> IO()
returnNka plg = do
  putStrLn "Printing NKA"
  print $ getFinalNka (getUpdatedPlg plg)
import Helper
import Data.Char
import System.IO (isEOF)
import PlgTypes
import System.Environment ( getArgs )

main :: IO ()
main = do
    (flag, input) <- processArguments =<< getArgs
    
    either end flag (check input)

getInput :: String -> IO String
getInput "" = getContents
getInput filename = readFile filename

parseArguments :: [String] -> IO(String, String)
parseArguments [flag] =             return (flag, "")
parseArguments [flag, filename] =   return (flag, filename)
parseArguments others =             end "Error in arguments. Expecting arguments in format -i|-1|-2 [FILE]"

processArguments :: [String] -> IO([Char] -> IO(), String)
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
    
returnPlg :: [Char] -> IO()
returnPlg plg = do
  putStrLn "Printing base PLG"
  print plg

returnUpdatedPlg :: [Char] -> IO()
returnUpdatedPlg plg = do
  putStrLn "Printing updated PLG"
  print plg

returnNka :: [Char] -> IO()
returnNka plg = do
  putStrLn "Printing NKA"
  print plg

check :: String -> Either String [Char]
check x = if 1 == 2 then Left "Test" else Right x
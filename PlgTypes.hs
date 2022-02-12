module PlgTypes where

import Data.List (intercalate)

type Nonterminals = [String]
type Terminals = [String]
type StartNonterminal = String
type Rules = [String]
type Rule = String


data PlgGrammar = PlgGrammar Nonterminals Terminals StartNonterminal Rules deriving (Eq)

instance Show PlgGrammar where
    show grammar@(PlgGrammar nonterminals terminals startNonterminal rules) = intercalate "\n" $ [(intercalate "," $ nonterminals), (intercalate "," $ terminals), startNonterminal] ++ rules 

getNonterminals :: PlgGrammar -> Nonterminals
getNonterminals (PlgGrammar nonterminals _ _ _) = nonterminals

getTerminals :: PlgGrammar -> Terminals
getTerminals (PlgGrammar _ terminals _ _) = terminals

getStartNonterminal :: PlgGrammar -> StartNonterminal
getStartNonterminal (PlgGrammar _ _ startNonterminal _) = startNonterminal

getRules :: PlgGrammar -> Rules
getRules (PlgGrammar _ _ _ rules) = rules

getRuleLeftSide :: [Char] -> [Char]
getRuleLeftSide rule = takeWhile (/= '-') rule

getRuleRightSide :: [Char] -> [Char]
getRuleRightSide rule = tail $ dropWhile (/= '>') rule
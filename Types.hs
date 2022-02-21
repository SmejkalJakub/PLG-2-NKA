module Types where

import Data.List (intercalate)

type Rules = [String]
---------------------------------PLG Grammar--------------------------------------------
type Nonterminals = [String]
type Terminals = [String]
type StartNonterminal = String
type Rule = String

data PlgGrammar = PlgGrammar Nonterminals Terminals StartNonterminal Rules

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

getRulesForNonTerminal :: PlgGrammar -> [Char] -> Rules
getRulesForNonTerminal grammar nonterminal = [x | x <- (getRules grammar), (getRuleLeftSide x == nonterminal)]
---------------------------------END PLG Grammar--------------------------------------------
------------------------------------NKA-----------------------------------------------------

type States = [String]
type Sigma = [String]
type StartState = String
type EndStates = [String]

data NKA = NKA States Sigma StartState EndStates Rules

instance Show NKA where
    show nka@(NKA states sigma startState endStates rules) = intercalate "\n" $ [(intercalate "," $ states), (intercalate "" $ sigma), startState, (intercalate "," $ endStates)] ++ rules 

getStates :: NKA -> States
getStates (NKA states _ _ _ _) = states

getSigma :: NKA -> Terminals
getSigma (NKA _ sigma _ _ _) = sigma

getStartState :: NKA -> StartState
getStartState (NKA _ _ startState _ _) = startState

getEndStates :: NKA -> EndStates
getEndStates (NKA _ _ _ endStates _) = endStates

getNKARules :: NKA -> Rules
getNKARules (NKA _ _ _ _ rules) = rules
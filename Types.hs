-- Project: plg-2-nka
-- Author: Jakub Smejkal (xsmejk28)
-- Year: 2022

-- Module containing PLG and NKA types and some functions to work with them
module Types where

import Data.List (intercalate)

type Rules = [String]
---------------------------------PLG Grammar--------------------------------------------
type Nonterminals = [String]
type Terminals = [String]
type StartNonterminal = String

-- PLG grammar type
data PlgGrammar = PlgGrammar Nonterminals Terminals StartNonterminal Rules

-- Implementation of show for the PlgGrammar type so it can be printed out
instance Show PlgGrammar where
    show grammar@(PlgGrammar nonterminals terminals startNonterminal rules) = intercalate "\n" $ [(intercalate "," $ nonterminals), (intercalate "," $ terminals), startNonterminal] ++ rules 

-- Function that returns all the nonterminals of the grammar
getNonterminals :: PlgGrammar -> Nonterminals
getNonterminals (PlgGrammar nonterminals _ _ _) = nonterminals

-- Function that returns all the terminals of the grammar
getTerminals :: PlgGrammar -> Terminals
getTerminals (PlgGrammar _ terminals _ _) = terminals

-- Function that returns the start nonterminal of the grammar
getStartNonterminal :: PlgGrammar -> StartNonterminal
getStartNonterminal (PlgGrammar _ _ startNonterminal _) = startNonterminal

-- Function that returns all the rules of the grammar
getRules :: PlgGrammar -> Rules
getRules (PlgGrammar _ _ _ rules) = rules

-- Function that returns left side of given rule
getRuleLeftSide :: [Char] -> [Char]
getRuleLeftSide rule = takeWhile (/= '-') rule

-- Function that returns right side of given rule
getRuleRightSide :: [Char] -> [Char]
getRuleRightSide rule = tail $ dropWhile (/= '>') rule

-- Function that returns all rules for given nonterminal
getRulesForNonTerminal :: PlgGrammar -> [Char] -> Rules
getRulesForNonTerminal grammar nonterminal = [x | x <- (getRules grammar), (getRuleLeftSide x == nonterminal)]
---------------------------------END PLG Grammar--------------------------------------------
------------------------------------NKA-----------------------------------------------------

type States = [String]
type Sigma = [String]
type StartState = String
type EndStates = [String]

-- NKA type
data NKA = NKA States Sigma StartState EndStates Rules

-- Implementation of show for the NKA type so it can be printed out
instance Show NKA where
    show nka@(NKA states sigma startState endStates rules) = intercalate "\n" $ [(intercalate "," $ states), (intercalate "" $ sigma), startState, (intercalate "," $ endStates)] ++ rules 

-- Function that returns all the states of the NKA
getStates :: NKA -> States
getStates (NKA states _ _ _ _) = states

-- Function that returns all symbols in sigma for the NKA
getSigma :: NKA -> Terminals
getSigma (NKA _ sigma _ _ _) = sigma

-- Function that returns the start state
getStartState :: NKA -> StartState
getStartState (NKA _ _ startState _ _) = startState

-- Function that returns all the end states of the NKA
getEndStates :: NKA -> EndStates
getEndStates (NKA _ _ _ endStates _) = endStates

-- Function that returns all the rules of the NKA
getNKARules :: NKA -> Rules
getNKARules (NKA _ _ _ _ rules) = rules
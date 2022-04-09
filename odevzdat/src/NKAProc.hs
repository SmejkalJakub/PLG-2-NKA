-- Project: plg-2-nka
-- Author: Jakub Smejkal (xsmejk28)
-- Year: 2022

-- Module containing implementation of PLG-2-NKA parsing
module NKAProc where

import Types
import Helpers (trim, parseToRulesArray)
import PlgProc (groupRulesForNonterminal)
import Data.List (intercalate)

--------------------- Flag -2 ---------------------------

-- Function that transfers grammar nonterminals into tuple where each nonterminal has a specific number assigned to it
getNonTerminalsTuple :: [String] -> Int -> [([Char], [Char])]
getNonTerminalsTuple [] _ = []
getNonTerminalsTuple (x:[]) index = [(x, (show index))]
getNonTerminalsTuple (x:xs) index = (x, (show index)) : getNonTerminalsTuple xs (index + 1) 

-- Function that transfers list of grammar rules to the NKA rules
transferRules :: PlgGrammar -> [([Char], [Char])] -> [[[Char]]]
transferRules grammar statesTuple = map (`getNkaRules` statesTuple) (map (`groupRulesForNonterminal` grammar) (getNonterminals grammar))

-- Function returns all the rules updated to the NKA format
getNkaRules :: [Char] -> [([Char], [Char])] -> [[Char]]
getNkaRules rule statesTuple = parseToNkaRules (getNumberForNonterminal (getRuleLeftSide rule) statesTuple) statesTuple (getRuleRightSide rule)
    
-- Function that parses all the rules for one nonterminal into the NKA rules
parseToNkaRules :: [Char] -> [([Char], [Char])] -> [Char] -> [[Char]]
parseToNkaRules _ _ [] = []

parseToNkaRules state statesTuple rules = do 
    if ruleLen == 1 
        then 
            [] : parseToNkaRules state statesTuple restOfTheRules 
            else intercalate "," [state, [head currentRule], getNumberForNonterminal (tail currentRule) statesTuple] : parseToNkaRules state statesTuple restOfTheRules
    where
        currentRule = do if length (takeWhile (/= ' ') rules) == 0 then rules else takeWhile (/= ' ') rules
        restOfTheRules = do if length (dropWhile (/= ' ') rules) == 0 then [] else tail $ dropWhile (/= ' ') rules
        ruleLen = length currentRule

-- Get the number that belongs to the given letter based on the tuple
getNumberForNonterminal :: [Char] -> [([Char], [Char])] -> [Char]
getNumberForNonterminal _ [] = ['X']
getNumberForNonterminal nonterminal (x:xs) = if (fst x) == nonterminal then snd x else getNumberForNonterminal nonterminal xs

-- Function that returns the final desired NKA for the input grammar
getFinalNka :: PlgGrammar -> NKA
getFinalNka grammar = NKA (map snd statesTuple) (getTerminals grammar) (startState) (endStates) (rules)
    where 
        statesTuple = getNonTerminalsTuple (getNonterminals grammar) 1 
        startState = getNumberForNonterminal (trim(getStartNonterminal grammar)) statesTuple
        endStates = map (`getNumberForNonterminal` statesTuple) (map getRuleLeftSide [x | x <- (getRules grammar), '#' `elem` x])
        rules = init $ parseToRulesArray $ transferRules grammar statesTuple

--------------------- END Flag -2 ---------------------------

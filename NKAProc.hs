module NKAProc where

import Types
import Helpers
import PlgProc

import Data.List (intercalate)

getNonTerminalsTuple :: [String] -> Int -> [([Char], [Char])]
getNonTerminalsTuple (x:[]) index = [(x, (show index))]
getNonTerminalsTuple (x:xs) index = (x, (show index)) : getNonTerminalsTuple xs (index + 1) 

transferRules :: PlgGrammar -> [([Char], [Char])] -> [[[Char]]]
transferRules grammar statesTuple = map (`getNkaRules` statesTuple) (map (`groupRulesForNonterminal` grammar) (getNonterminals grammar))

getNkaRules :: [Char] -> [([Char], [Char])] -> [[Char]]
getNkaRules rule statesTuple = parseToNkaRules (getNumberForNonterminal (getRuleLeftSide rule) statesTuple) statesTuple (getRuleRightSide rule)
    
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

getNumberForNonterminal :: [Char] -> [([Char], [Char])] -> [Char]
getNumberForNonterminal _ [] = ['X']
getNumberForNonterminal nonterminal (x:xs) = if (fst x) == nonterminal then snd x else getNumberForNonterminal nonterminal xs

getFinalNka :: PlgGrammar -> NKA
getFinalNka grammar = NKA (map snd statesTuple) (getTerminals grammar) (startState) (endStates) (rules)
    where 
        statesTuple = getNonTerminalsTuple (getNonterminals grammar) 1 
        startState = getNumberForNonterminal (trim(getStartNonterminal grammar)) statesTuple
        endStates = map (`getNumberForNonterminal` statesTuple) (map getRuleLeftSide [x | x <- (getRules grammar), '#' `elem` x])
        rules = parseToRulesArray $ transferRules grammar statesTuple

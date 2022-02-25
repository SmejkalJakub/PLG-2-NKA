-- Project: plg-2-nka
-- Author: Jakub Smejkal (xsmejk28)
-- Year: 2022

-- Module containing functions for updated grammar from the input grammar
module PlgProc where

import Types
import Helpers
import Data.Char
import Data.List

--------------------- Flag -1 ---------------------------

-- Function that returns all the updated rules so they are in correct format for desired grammar
updateRules :: PlgGrammar -> [[[Char]]]
updateRules grammar = do
    if all isAllRulesInCorrectFormatForUpdatedGrammar rules 
        then 
            [rules] 
        else 
            map getUpdatedRules (map (`groupRulesForNonterminal` grammar) (getNonterminals grammar))
    where 
        rules = getRules grammar

-- Function that takes all rules for nonterminal and groups them into one string
groupRulesForNonterminal :: [Char] -> PlgGrammar -> [Char]
groupRulesForNonterminal nonterminal grammar = nonterminal ++ "->" ++ intercalate " " (map getRuleRightSide (map trim (getRulesForNonTerminal grammar nonterminal)))

-- Function returns all the updated rules for all the nonterminals
getUpdatedRules :: [Char] -> [[Char]]
getUpdatedRules rule = expandRules (getRuleLeftSide rule) (getRuleRightSide rule) 0 0

-- Function that parses all rules for nonterminal into the correct format
expandRules :: [Char] -> [Char] -> Integer -> Integer -> [[Char]]
expandRules _ [] _ _ = []

expandRules nonterminal rules 0 maxindex = do
    if ruleLen == 1 && currentRule == "#" || (ruleLen == 2 && isUpper (last currentRule))
        then 
            (nonterminal ++ "->" ++ currentRule) : 
            expandRules nonterminal restOfTheRules 0 maxindex
    else 
        if ruleLen == 1 && all isLower currentRule 
            then 
                (nonterminal ++ "->" ++ currentRule ++ nonterminal ++ (show $ maxindex + 1)) : 
                (nonterminal ++ (show $ maxindex + 1) ++ "->#") : 
                expandRules nonterminal restOfTheRules 0 (maxindex + 1)
        else 
            (nonterminal ++ "->" ++ (take 1 currentRule) ++ nonterminal ++ (show $ maxindex + 1)) : 
            expandRules nonterminal (tail rules) (maxindex + 1) (maxindex + 1)
        where
            currentRule = do if length (takeWhile (/= ' ') rules) == 0 then rules else takeWhile (/= ' ') rules
            restOfTheRules = do if length (dropWhile (/= ' ') rules) == 0 then [] else tail $ dropWhile (/= ' ') rules
            ruleLen = length currentRule

expandRules nonterminal rules index maxindex = do
    if ruleLen == 1 && currentRule == "#" || (ruleLen == 2 && isUpper (last currentRule))
        then 
            (nonterminal ++ show index ++ "->" ++ currentRule) : 
            expandRules nonterminal restOfTheRules 0 maxindex
    else
        if ruleLen == 1 && all isLower currentRule
            then 
                (nonterminal ++ (show index) ++ "->" ++ currentRule ++ nonterminal ++ (show $ index + 1)) : 
                (nonterminal ++ (show $ index + 1) ++ "->#") : 
                expandRules nonterminal restOfTheRules 0 (maxindex + 1)
        else 
            (nonterminal ++ (show index) ++ "->" ++ (take 1 currentRule) ++ nonterminal ++ (show $ index + 1)) : 
            expandRules nonterminal (tail rules) (index + 1) (maxindex + 1)
        where
            currentRule = do if length (takeWhile (/= ' ') rules) == 0 then rules else takeWhile (/= ' ') rules
            restOfTheRules = do if length (dropWhile (/= ' ') rules) == 0 then [] else tail $ dropWhile (/= ' ') rules
            ruleLen = length currentRule

-- Function that checks if all the rules are in a correct format -> nothing has to be done
isAllRulesInCorrectFormatForUpdatedGrammar :: [Char] -> Bool
isAllRulesInCorrectFormatForUpdatedGrammar rule = do 
    if length rightSide == 2 && isLower (rightSide !! 0) && isUpper (rightSide !! 1) ||
       length rightSide == 1 && rightSide !! 0 == '#'
        then True
        else False
    where
        rightSide = trim(getRuleRightSide rule)

-- Function that changes input into the updated grammar that is ready to be parsed into NKA
getUpdatedPlg :: PlgGrammar -> PlgGrammar
getUpdatedPlg grammar = PlgGrammar (nub $ (updatedNonterminals ++ getNonterminals grammar)) (getTerminals grammar) (getStartNonterminal grammar) (rules)
    where 
        rules = parseToRulesArray $ updateRules grammar
        updatedNonterminals = nub $ map getRuleLeftSide rules

--------------------- END Flag -1 ---------------------------

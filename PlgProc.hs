module PlgProc where

import Helper
import PlgTypes

import Data.Char
import Data.List

updateRules :: PlgGrammar -> [[[Char]]]
updateRules grammar = do
    if all isAllRulesInCorrectFormatForUpdatedGrammar rules then [rules] else map (`getUpdatedRules` grammar) (map (`groupRulesForNonterminal` grammar) (getNonterminals grammar))
    where 
        rules = getRules grammar

groupRulesForNonterminal :: [Char] -> PlgGrammar -> [Char]
groupRulesForNonterminal nonterminal grammar = nonterminal ++ "->" ++ intercalate " " (map getRuleRightSide (map trim (getRulesForNonTerminal grammar nonterminal)))

getUpdatedRules :: [Char] -> PlgGrammar -> [[Char]]
getUpdatedRules rule grammar = init (expandRules (getRuleLeftSide rule) (getRuleRightSide rule) 0 0)

expandRules :: [Char] -> [Char] -> Integer -> Integer -> [[Char]]
expandRules nonterminal ['-'] index maxindex = [""]

expandRules nonterminal rules 0 maxindex = do
    if ruleLen == 1 && currentRule == "#" || (ruleLen == 2 && isUpper (last currentRule))
        then (nonterminal ++ "->" ++ currentRule) : expandRules nonterminal restOfTheRules 0 maxindex
    else 
        if ruleLen == 1 && all isLower currentRule 
            then 
                (nonterminal ++ "->" ++ currentRule ++ nonterminal ++ (show (maxindex + 1))) : (nonterminal ++ (show (maxindex + 1)) ++ "->#") : expandRules nonterminal restOfTheRules 0 (maxindex + 1)
        else 
            (nonterminal ++ "->" ++ (take 1 currentRule) ++ nonterminal ++ (show (maxindex + 1))) : expandRules nonterminal (tail rules) (maxindex + 1) (maxindex + 1)
        where
            currentRule = do if length (takeWhile (/= ' ') rules) == 0 then rules else takeWhile (/= ' ') rules
            restOfTheRules = do if length (dropWhile (/= ' ') rules) == 0 then ['-'] else drop 1 (dropWhile (/= ' ') rules)
            ruleLen = length currentRule

expandRules nonterminal rules index maxindex = do
    if ruleLen == 1 && currentRule == "#" || (ruleLen == 2 && isUpper (last currentRule))
        then 
            (nonterminal ++ show index ++ "->" ++ currentRule) : expandRules nonterminal restOfTheRules 0 maxindex
    else
        if ruleLen == 1 && all isLower currentRule
            then 
                (nonterminal ++ (show index) ++ "->" ++ currentRule ++ nonterminal ++ (show (index + 1))) : (nonterminal ++ (show (index + 1)) ++ "->#") : expandRules nonterminal restOfTheRules 0 (maxindex + 1)
        else 
            (nonterminal ++ (show index) ++ "->" ++ (take 1 currentRule) ++ nonterminal ++ (show (index + 1))) : expandRules nonterminal (tail rules) (index + 1) (maxindex + 1)
        where
            currentRule = do if length (takeWhile (/= ' ') rules) == 0 then rules else takeWhile (/= ' ') rules
            restOfTheRules = do if length (dropWhile (/= ' ') rules) == 0 then ['-'] else drop 1 (dropWhile (/= ' ') rules)
            ruleLen = length currentRule

isAllRulesInCorrectFormatForUpdatedGrammar :: [Char] -> Bool
isAllRulesInCorrectFormatForUpdatedGrammar rule = do 
    if length rightSide == 2 && isLower (rightSide !! 0) && isUpper (rightSide !! 1) ||
       length rightSide == 1 && rightSide !! 0 == '#'
        then True
        else False
    where
        rightSide = trim(getRuleRightSide rule)

updateGrammarNonterminals :: PlgGrammar -> Nonterminals
updateGrammarNonterminals grammar = getNonterminals grammar

parseToRulesArray :: [[a]] -> [a]
parseToRulesArray rule = concat rule

getUpdatedPlg :: PlgGrammar -> PlgGrammar
getUpdatedPlg grammar = PlgGrammar (updatedNonterminals) (getTerminals grammar) (getStartNonterminal grammar) (rules)
    where 
        rules = parseToRulesArray $ updateRules grammar
        updatedNonterminals = nub $ map getRuleLeftSide rules
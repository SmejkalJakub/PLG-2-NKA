
module PlgParser where

import PlgTypes
import Helper

import Data.Char
import Data.List

createGrammar :: String -> PlgGrammar
createGrammar input = PlgGrammar (getListNonterminals input) (getListTerminals input) (getStartN input) (getListRules input)

getListNonterminals :: String -> [String]
getListNonterminals input = nub $ map parseToString $ [x | x <- lines input !! 0, isUpper x]

getListTerminals :: String -> [String]
getListTerminals input = nub $ map parseToString $ [x | x <- lines input !! 1, isLower x]

getStartN :: String -> String
getStartN input = lines input !! 2

getListRules :: String -> [String]
getListRules input = nub $ drop 3 $ lines input

validateNonTerminals :: [Char] -> Bool
validateNonTerminals nonTerminals = all (== True) [all validateNonTerminal (trim(nonTerminals))]

validateNonTerminal :: Char -> Bool
validateNonTerminal nonTerminal = do 
    if isUpper nonTerminal || nonTerminal == ',' then True else False 

validateTerminals :: [Char] -> Bool
validateTerminals terminals = all (== True) [all validateTerminal (trim(terminals))]

validateTerminal :: Char -> Bool
validateTerminal terminal = do 
    if isLower terminal || terminal == ',' then True else False

validateStartNonTerminal :: [Char] -> PlgGrammar -> Bool
validateStartNonTerminal startNonTerminal grammar = do
    let nonterminals = concat (getNonterminals grammar)
    if startNonTerminal !! 0 `elem` nonterminals && length startNonTerminal == 2 then True else False

validateRules :: [String] -> PlgGrammar -> Bool
validateRules rules grammar = do
    all (== True) [all (`validateRule` grammar) rules]

validateRule :: [Char] -> PlgGrammar -> Bool
validateRule rule grammar = do
    if length rule >= 4 && rule !! 1 == '-' && rule !! 2 == '>' && 
        validateRuleLeftSide (trim(getRuleLeftSide rule)) grammar && 
        validateRuleRightSide (trim(getRuleRightSide rule)) grammar 
        then True
        else False

validateRuleLeftSide :: [Char] -> PlgGrammar -> Bool
validateRuleLeftSide leftSide grammar = do
    let nonterminals = getNonterminals grammar
    if leftSide `elem` nonterminals && length leftSide == 1 then True else False 

validateRuleRightSide :: [Char] -> PlgGrammar -> Bool
validateRuleRightSide rightSide grammar = do
    if (length rightSide == 1 && (rightSide !! 0) /= '#' && not (all isLower rightSide)) ||
       ("#" `isInfixOf` rightSide && length rightSide > 1) ||
       (length rightSide > 1 && ((not (all isLower rightSide) || not (isUpper (last rightSide))) && not (all isLower (init rightSide))))
          then False
    else
        all (`validateRuleRightSideSymbol` grammar) rightSide

validateRuleRightSideSymbol :: Char -> PlgGrammar -> Bool
validateRuleRightSideSymbol symbol grammar = do
    let nonterminals = getNonterminals grammar
    let terminals = getTerminals grammar
    if [symbol] `elem` terminals || [symbol] `elem` nonterminals || symbol == '#' then True else False

validateInput :: String -> PlgGrammar -> Bool
validateInput input grammar = if all (== True) [validateNonTerminals (lines input !! 0), 
    validateTerminals(lines input !! 1), 
    validateStartNonTerminal(lines input !! 2) grammar,
    validateRules(drop 3 $ lines input) grammar] then True
    else False

isGrammarValid :: String -> PlgGrammar -> Bool
isGrammarValid input grammar = validateInput input grammar

createAndCheckGrammar :: String -> Either String PlgGrammar
createAndCheckGrammar input = if check then Right grammar else Left "Grammar not valid"
    where
        grammar = createGrammar input
        check@(checkBool) = isGrammarValid input grammar
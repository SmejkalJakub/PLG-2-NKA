-- Project: plg-2-nka
-- Author: Jakub Smejkal (xsmejk28)
-- Year: 2022

-- Module containing functions for creating and validating grammar from input
module PlgParser where

import Types
import Helpers (parseToString, trim)
import Data.Char
import Data.List

-- Function that creates grammar from the input string
createGrammar :: String -> PlgGrammar
createGrammar input = PlgGrammar (getListNonterminals input) (getListTerminals input) (getStartN input) (getListRules input)

-- Function that returns all the nonterminals that should be on the first line of the input. Also removes duplicates
getListNonterminals :: String -> [String]
getListNonterminals input = nub $ map parseToString $ [x | x <- lines input !! 0, isUpper x]

-- Function that returns all the terminals that should be on the second line of the input. Also removes duplicates
getListTerminals :: String -> [String]
getListTerminals input = nub $ map parseToString $ [x | x <- lines input !! 1, isLower x]

-- Function that returns the start nonterminal that should be on the third line of the input
getStartN :: String -> String
getStartN input = lines input !! 2

-- Function that returns the rest of the input where the rules should be and removes the duplicates
getListRules :: String -> [String]
getListRules input = nub $ drop 3 $ map trim (lines input)

-- Function that tries to validate all the nonterminals
validateNonTerminals :: [Char] -> Bool
validateNonTerminals nonTerminals = all (== True) [all validateNonTerminal (trim(nonTerminals))]

-- Function that validates each nonterminal
validateNonTerminal :: Char -> Bool
validateNonTerminal nonTerminal = do 
    if isUpper nonTerminal || nonTerminal == ',' then True else False 

-- Function that tries to validate all the terminal
validateTerminals :: [Char] -> Bool
validateTerminals terminals = all (== True) [all validateTerminal (trim(terminals))]

-- Function that validates each terminal
validateTerminal :: Char -> Bool
validateTerminal terminal = do 
    if isLower terminal || terminal == ',' then True else False

-- Function that validates start nonterminal
validateStartNonTerminal :: [Char] -> PlgGrammar -> Bool
validateStartNonTerminal startNonTerminal grammar = do
    let nonterminals = concat (getNonterminals grammar)
    if startNonTerminal !! 0 `elem` nonterminals && length startNonTerminal == 2 then True else False

-- Function that tries to validate all the rules
validateRules :: [String] -> PlgGrammar -> Bool
validateRules rules grammar = do
    all (== True) [all (`validateRule` grammar) rules]

-- Function that validates each rule
validateRule :: [Char] -> PlgGrammar -> Bool
validateRule rule grammar = do
    if length rule >= 4 && rule !! 1 == '-' && rule !! 2 == '>' && 
        validateRuleLeftSide (trim(getRuleLeftSide rule)) grammar && 
        validateRuleRightSide (trim(getRuleRightSide rule)) grammar 
        then True
        else False

-- Function that validates left side of each rule
validateRuleLeftSide :: [Char] -> PlgGrammar -> Bool
validateRuleLeftSide leftSide grammar = do
    let nonterminals = getNonterminals grammar
    if leftSide `elem` nonterminals && length leftSide == 1 then True else False 

-- Function that validates right side of each rule
validateRuleRightSide :: [Char] -> PlgGrammar -> Bool
validateRuleRightSide rightSide grammar = do
    if length rightSide == 0 ||
       (length rightSide == 1 && (rightSide !! 0) /= '#' && not (all isLower rightSide)) ||
       ("#" `isInfixOf` rightSide && length rightSide > 1) ||
       (length rightSide > 1 && ((not (all isLower rightSide) || not (isUpper (last rightSide))) && not (all isLower (init rightSide))))
          then False
    else
        all (`validateRuleRightSideSymbol` grammar) rightSide

-- Function that validates each symbol on the right side of each rule
validateRuleRightSideSymbol :: Char -> PlgGrammar -> Bool
validateRuleRightSideSymbol symbol grammar = do
    let nonterminals = getNonterminals grammar
    let terminals = getTerminals grammar
    if [symbol] `elem` terminals || [symbol] `elem` nonterminals || symbol == '#' then True else False

-- Function that validates all the elements of the inputed grammar
validateInput :: String -> PlgGrammar -> Bool
validateInput input grammar = if all (== True) [validateNonTerminals (lines input !! 0), 
    validateTerminals(lines input !! 1), 
    validateStartNonTerminal(lines input !! 2) grammar,
    validateRules(drop 3 $ lines input) grammar] then True
    else False

-- Function that creates and checks the grammar from the input string. If grammar is not valid, error message will be returned
createAndCheckGrammar :: String -> Either String PlgGrammar
createAndCheckGrammar input = if validateInput input grammar then Right grammar else Left "Grammar not valid"
    where
        grammar = createGrammar input
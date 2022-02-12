module PlgProc where

import Helper
import PlgTypes

import Data.Char
import Data.List

updateRules :: PlgGrammar -> Rules
updateRules grammar = map (checkRule) (getRules grammar)

checkRule :: Rule -> Rule
checkRule rule = if (all isLower rule) then "TEST" else rule ++ rule

--expandRulesWithNonterminal Rule -> Rule
--expandRulesWithNonterminal rule = 
    
getUpdatedPlg :: PlgGrammar -> PlgGrammar
getUpdatedPlg grammar = PlgGrammar (getNonterminals grammar) (getTerminals grammar) (getStartNonterminal grammar) (updateRules grammar)

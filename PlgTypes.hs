module PlgTypes where

import Data.List (intercalate)

type Nonterminals = [String]
type Terminals = [String]
type StartNonterminal = String
type Rules = [String]

data PlgGrammar = PlgGrammar Nonterminals Terminals StartNonterminal Rules deriving (Eq)

getNonterminals :: PlgGrammar -> Nonterminals
getNonterminals (PlgGrammar nonterminals _ _ _) = nonterminals

getTerminals :: PlgGrammar -> Terminals
getTerminals (PlgGrammar _ terminals _ _) = terminals

getStartNonterminal :: PlgGrammar -> StartNonterminal
getStartNonterminal (PlgGrammar _ _ startNonterminal _) = startNonterminal

getRules :: PlgGrammar -> Rules
getRules (PlgGrammar _ _ _ rules) = rules
module MatchPattern(matchPattern) where

import qualified Data.Map as M
import Text.EditDistance

import ActionMatch

matchPattern :: String -> ActionPattern -> ActionMatch
matchPattern phrase (ActionPattern pattern action) =
             ActionMatch conf args action
             where args = M.fromList [("phrase", phrase)]
                   conf = strMatch phrase pattern



strMatch :: String -> String -> Float
strMatch a b = 1.0 - fromIntegral(levenshteinDistance defaultEditCosts a b) / (fromIntegral . maximum $ [length a, length b])                   

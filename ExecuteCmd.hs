module ExecuteCmd(executeCmd) where

import OSActions

import Data.Map as M
import qualified Data.Text as T

import Text.EditDistance

import Data.Maybe

data ActionPattern = ActionPattern {
     pattern :: String,
     patternAction :: OSAction
     }

data ActionMatch = ActionMatch {
     confidence :: Float,
     args :: ActionArgs,
     matchAction :: OSAction
     }     
     
actionPatterns ::[ActionPattern]
actionPatterns = 
               [
               ActionPattern "this is a test" echo,
               ActionPattern "suck my balls" echo2
               ]               

executeCmd :: T.Text -> Float -> [T.Text] -> IO ()
executeCmd cmd confidence alternatives =
           action args
           where results = Prelude.map (matchPattern . show $ cmd) actionPatterns
                 ActionMatch _ args action = head . id $ results

matchPattern :: String -> ActionPattern -> ActionMatch
matchPattern query (ActionPattern pattern action) =
             ActionMatch 1.0 args action
             where args = M.fromList [("phrase", query)]
   
{-
 iterate through action patterns
 find matching percentages
 sort
 take first

 multiply confidences
 if < threshold
 repeat task for alternatives
 
-}
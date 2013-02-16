module ExecuteCmd(executeCmd) where

import qualified Data.List as L
import qualified Data.Text as T
import qualified Data.Map as M

import Data.Maybe

import ActionMatch
import MatchPattern
import ActionPatterns

executeCmd :: T.Text -> Float -> [T.Text] -> IO ()
executeCmd cmd confidence alternatives =
           action args
           where results = map (matchPattern . show $ cmd) actionPatterns
                 ActionMatch confidence args actionName = head . L.sort $ results
                 action = fromJust(M.lookup actionName actions)

{-
 iterate through action patterns
 match patterns
 sort
 take first

 TODO
 multiply confidences
 if < threshold
 repeat task for alternatives

 check errors, etc
-}
module ActionMatch(
ActionArgs,
OSAction,
ActionPattern(..),
ActionMatch(..)
)where

import qualified Data.Map as M

type ActionArgs = M.Map String String
type OSAction = ActionArgs -> IO()

data ActionPattern = ActionPattern {
     pattern :: String,
     patternActionName :: String
     } deriving(Eq, Show)

data ActionMatch = ActionMatch {
     confidence :: Float,
     args :: ActionArgs,
     matchActionName :: String
     } deriving(Eq, Show)

-- sorts by most relevant(highest confidence) first         
instance Ord ActionMatch where
         (<=) (ActionMatch a _ _) (ActionMatch b _ _) = (<=) b a

module ActionMatch(
ActionArgs,
emptyArgs,
OSAction,
ActionPattern(..),
ActionMatch(..),
MatchSection(..)
)where

import qualified Data.Map as M

type ActionArgs = M.Map String String
emptyArgs :: ActionArgs
emptyArgs = M.empty :: ActionArgs

type OSAction = ActionArgs -> IO()

data ActionPattern = ActionPattern {
     pattern :: String,
     patternActionName :: String
     } deriving(Eq, Show)

data MatchSection = SectionPhrase String | SectionVar String deriving (Eq, Show)
     
data ActionMatch = ActionMatch {
     matchPhrase :: String,
     sections :: [MatchSection],
     confidence :: Float,
     args :: ActionArgs,
     matchActionName :: String
     } deriving(Eq, Show)

-- sorts by most relevant(highest confidence) first         
instance Ord ActionMatch where
         (<=) (ActionMatch _ _ a _ _) (ActionMatch _ _ b _ _) = (<=) b a

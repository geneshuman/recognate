module OSActions(
ActionArgs,
OSAction,
echo
) where

import System.IO
import Data.Maybe

import Data.Map as M

type ActionArgs = M.Map String String
type OSAction = ActionArgs -> IO()

echo :: OSAction
echo args = putStrLn . fromJust $ (M.lookup "phrase" args)

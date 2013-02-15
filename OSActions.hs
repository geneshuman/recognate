module OSActions(
ActionArgs,
OSAction,
echo,
echo2
) where

import System.IO
import Data.Maybe

import Data.Map as M

type ActionArgs = M.Map String String
type OSAction = ActionArgs -> IO()

echo :: OSAction
echo args = putStrLn . fromJust $ (M.lookup "phrase" args)

echo2 :: OSAction
echo2 args = putStrLn . reverse . fromJust $ (M.lookup "phrase" args)

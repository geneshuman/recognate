module OSActions(
echo,
echo2
) where

import ActionMatch

import System.IO
import Data.Maybe

import qualified Data.Map as M

-- implementation of actions --

echo :: OSAction
echo args = putStrLn . fromJust $ (M.lookup "cmd" args)

echo2 :: OSAction
echo2 args = putStrLn . reverse . fromJust $ (M.lookup "cmd" args)



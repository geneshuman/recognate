module ExecuteCmd(executeCmd) where

import OSActions

import Data.Map as M
import qualified Data.Text as T

import Text.EditDistance

import Data.Maybe

actionPatterns :: M.Map String OSAction
actionPatterns = M.fromList
               [
               ("*", echo)
               ]               


executeCmd :: T.Text -> Float -> [T.Text] -> IO ()
executeCmd cmd confidence alternatives =
   action (M.fromList [("phrase", show cmd)])
   where action = fromJust (M.lookup "*" actionPatterns)

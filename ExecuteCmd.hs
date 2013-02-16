module ExecuteCmd(executeCmd) where

import qualified Data.List as L
import qualified Data.Text as T
import qualified Data.Map as M
import Data.List.Split(splitOn)

import Text.EditDistance

import Data.Maybe

import ActionMatch
import ActionPatterns

strMatch :: String -> String -> Float
strMatch a b = 1.0 - fromIntegral(levenshteinDistance defaultEditCosts a b) / (fromIntegral . maximum $ [length a, length b])


executeCmd :: T.Text -> Float -> [T.Text] -> IO ()
executeCmd cmd confidence alternatives =
            (putStrLn . show $ initialMatches) >> (action newArgs)
--            action newArgs
            where action = fromJust(M.lookup "echo" actions)                         
                  ActionMatch _ _ _ args actionName = head results
                  results = processMatches initialMatches
                  initialMatches = initializeMatches (show cmd) (deMultiplexPatterns $ actionPatterns)
                  newArgs = M.insert "cmd" (show cmd) args                                 

-- expand blocks in patterns
deMultiplexPatterns :: [ActionPattern] -> [ActionPattern]
deMultiplexPatterns patterns = patterns


-- create initial match objects for algorithm
initializeMatches :: String -> [ActionPattern] -> [ActionMatch]
initializeMatches cmd patterns = map (initMatch cmd) patterns
                  where initMatch phrase pattern = case pattern of
                                  ActionPattern p actionName -> ActionMatch phrase (splitPattern p) 1 emptyArgs actionName


-- split pattern by '$' & interleave section phrases & section variables                                  
splitPattern :: String -> [MatchSection]
splitPattern p = zipWith (\a b -> a b) (cycle [(\p -> SectionPhrase p), (\p -> SectionVar p)]) bits
             where bits = map (\t -> T.unpack . T.strip . T.pack $ t) (filter (not . null) (splitOn "$" p))


processMatches :: [ActionMatch] -> [ActionMatch]
processMatches matches = [head matches]


{-
 TODO
 multiply confidences
 if < threshold
 repeat task for alternatives

 check errors, etc
-}
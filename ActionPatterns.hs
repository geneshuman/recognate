module ActionPatterns(actions, actionPatterns) where

import ActionMatch
import OSActions

import qualified Data.Map as M

actions :: M.Map String OSAction
actions = M.fromList [("echo", echo), ("echo2", echo2)]

actionPatterns ::[ActionPattern]
actionPatterns = 
               [
               ActionPattern "this is a test" "echo",
               ActionPattern "this is a chicken" "echo2"
               ]               

realActionPatterns ::[ActionPattern]
realActionPatterns = 
               [
               ActionPattern "*" "",                             
               ActionPattern "say $phrase" "",
               ActionPattern "play a random $artist album" "",
               ActionPattern "play a random album by $artist" "",               
               ActionPattern "play ($song|$album) by $artist" "",
               ActionPattern "play a random episode of $show" "",
               ActionPattern "play a random episode (from|of) season $season of $show" "",                              
               ActionPattern "(open|run|start) $application" "",               
               ActionPattern "pause" "",
               ActionPattern "resume" "",
               ActionPattern "rewind (by|) $time_interval" "",
               ActionPattern "(fast|)forward (by|) $time_interval" "",
               ActionPattern "stop" "",                              
               ActionPattern "google $phrase" "",
               ActionPattern "wikipedia $phrase" ""
               ]               
               






{-               
demultiplex action patterns
split patterns into chunks
match phrase against head of chunks
drop all phrases < matchThreshold

-}
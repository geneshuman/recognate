{-# LANGUAGE OverloadedStrings #-}

import AudioParseResult
import GoogleAudioParse

import Data.Maybe

main :: IO ()
main = do

     res <-  googleAudioFileParse "example.flac"
     putStrLn . show . getResult . fromJust $ res
     
     return ()



     
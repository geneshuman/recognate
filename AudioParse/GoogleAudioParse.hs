{-# LANGUAGE OverloadedStrings #-}

module GoogleAudioParse
(
googleAudioFileParse
) where

import System.Process
import System.IO
import Control.Applicative
import Control.Monad
import Data.Aeson
import qualified Data.HashMap.Strict  as HM
import qualified Data.ByteString.Lazy.Char8 as C
import qualified Data.Text as T

import Data.Maybe

import AudioParseResult

data GoogleAudioParseResult = GoogleAudioParseResult
     {
        status :: Int,
        id :: T.Text,
        hypotheses :: [GoogleSpeechHypothesis]
      } deriving(Show)

instance FromJSON GoogleAudioParseResult where
         parseJSON (Object v) = GoogleAudioParseResult <$>
                   v .: "status" <*>
                   v .: "id" <*>
                   (parseJSON =<< (v .: "hypotheses"))
         parseJSON _ = mzero

-- INCOMPLETE         
instance AudioParseResult GoogleAudioParseResult where
         getResult a = case hyp of
                    [] -> Left("Google Audio Parse Error - " ++ show (status a)) :: Either String T.Text
                    _ -> Right(utterance . head  $ hyp) :: Either String T.Text
                    where hyp = hypotheses(a)
         getConfidence a = case hyp of
                    [] -> Left("Google Audio Parse Error - " ++ show (status a)) :: Either String Float
                    _ -> Right(fromJust . confidence . head  $ hyp) :: Either String Float
                    where hyp = hypotheses(a)                    
         getAlternatives a = case hyp of
                    [] -> Left("Google Audio Parse Error - " ++ show (status a)) :: Either String [T.Text]
                    _ -> Right(tail(map (\e -> utterance e) hyp)) :: Either String [T.Text]
                    where hyp = hypotheses(a)                    
            
data GoogleSpeechHypothesis = GoogleSpeechHypothesis
     {
        utterance :: T.Text,
        confidence :: Maybe Float
      } deriving(Show)

instance FromJSON GoogleSpeechHypothesis where
         parseJSON (Object v) = GoogleSpeechHypothesis <$>
                   v .: "utterance" <*>
                   v .:? "confidence"
         parseJSON _ = mzero                                           

         
googleAudioFileParse :: String -> IO(Either String GoogleAudioParseResult)
googleAudioFileParse file = do

                     -- use curl directly from the command line
                     let cmd = "curl"
                     let url = "https://www.google.com/speech-api/v1/recognize?xjerr=1&client=chromium&pfilter=2&lang=en-US&maxresults=6"
                     let args = ["--data-binary","@" ++ file,"--header", "Content-type: audio/x-flac; rate=16000", url]
                     
                     (_, Just hout, _, _) <- createProcess (proc cmd args){ std_out = CreatePipe }
                     hSetBinaryMode hout False
                     apiJSON <- hGetContents hout
                     
                     let res = decode (C.pack apiJSON) :: Maybe GoogleAudioParseResult
                     case res of
                          Nothing -> return(Left("JSON parse error - " ++ apiJSON) :: Either String GoogleAudioParseResult)
                          Just r -> return(Right(r) :: Either String GoogleAudioParseResult)
                       
     



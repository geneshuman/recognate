{-# LANGUAGE OverloadedStrings #-}

module GoogleAudioParse
(
googleAudioFileParse
--GoogleAudioParseResult(..)
) where

import System.Process
import System.IO
import           Control.Applicative
import           Control.Monad
import           Data.Aeson
import qualified Data.HashMap.Strict  as HM
import qualified Data.ByteString.Lazy.Char8 as C
import qualified Data.Text as T

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

instance AudioParseResult GoogleAudioParseResult where
      getResult = utterance . head . hypotheses 
      getConfidence = confidence . head . hypotheses
      getAlternatives a = []
            
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

         
googleAudioFileParse :: String -> IO(Maybe GoogleAudioParseResult)
googleAudioFileParse file = do
                     let cmd = "curl"                                          
                     let args = ["--data-binary","@" ++ file,"--header", "Content-type: audio/x-flac; rate=16000","https://www.google.com/speech-api/v1/recognize?xjerr=1&client=chromium&pfilter=2&lang=en-US&maxresults=6"]
                     
                     (_, Just hout, _, _) <- createProcess (proc cmd args){ std_out = CreatePipe }
                     hSetBinaryMode hout False
                     apiJSON <- hGetContents hout
                     let res = decode (C.pack apiJSON) :: Maybe GoogleAudioParseResult
                     return res
                       
     

{-# LANGUAGE OverloadedStrings #-}

module GoogleAudioParse
(
googleAudioFileParseCurl,
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

import Network (withSocketsDo)
import Network.HTTP.Conduit
import Network.HTTP.Types.Header
import Network.HTTP.Types.Status
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString as BS
import Control.Monad.IO.Class (liftIO)

-- GoogleAudioParseResult
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

-- GoogleSpeechHypothesis                    
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


-- Audio Parsing Methods

-- use curl directly from the command line
googleAudioFileParseCurl :: String -> IO(Either String GoogleAudioParseResult)
googleAudioFileParseCurl file = do
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
                       
     
-- use http-conduit to stream the file to the server
googleAudioFileParse :: String -> IO(Either String GoogleAudioParseResult)
googleAudioFileParse file = withSocketsDo $ do
                     
                     contents <- L.readFile file 
                     
                     let url = "https://www.google.com/speech-api/v1/recognize?xjerr=1&client=chromium&pfilter=2&lang=en-US&maxresults=6"
                     apiJSON <- doGoogleAPIPost url contents

                     case apiJSON of
                          Left e -> return(Left(e) :: Either String GoogleAudioParseResult)
                          Right r -> let res = decode (r) :: Maybe GoogleAudioParseResult in
                                     case res of
                                          Nothing -> return(Left("JSON parse error - " ++ show r) :: Either String GoogleAudioParseResult)
                                          Just r -> return(Right(r) :: Either String GoogleAudioParseResult)
                       
-- helper method for ^^^^^^^^     
doGoogleAPIPost :: String -> L.ByteString -> IO(Either String L.ByteString)
doGoogleAPIPost url contents = do
                case parseUrl url of
                     Nothing -> return(Left("Invalid URL") :: Either String L.ByteString)
                     Just req -> withManager $ \manager -> do                
                          let reqHead = req { method = "POST",
                                              requestHeaders = [(hContentType, "audio/x-flac; rate=16000"),(hAccept,"*/*")],
                                              requestBody = RequestBodyLBS(contents)
                                            }
                          Response status _ _ src <- httpLbs reqHead manager
                          liftIO $ return(Right(src) :: Either String L.ByteString)






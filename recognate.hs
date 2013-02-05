import AudioParseResult
import GoogleAudioParse

import Data.Maybe

main :: IO ()
main = do

     res <-  googleAudioFileParse "example.flac"
     
     case res of
          Nothing -> putStrLn "No Result"
          _ -> putStrLn . show . getResult . fromJust $ res     
     
     return ()



     
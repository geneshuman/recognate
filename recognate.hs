import AudioParseResult
import GoogleAudioParse
--import ParseAction

-- parses an audio stream & attempts to convert the result into a system executable action
main :: IO ()
main = do

     res <- googleAudioFileParse "example.flac"
     
     case res of
          Left e -> putStrLn e
          Right r -> case getResult(r) of 
                    Left e -> putStrLn e
                    Right r2 ->(putStrLn . show $ r2)
     
     return ()



     
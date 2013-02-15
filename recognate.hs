import AudioParseResult
import GoogleAudioParse
import ExecuteCmd

import System.Environment  

-- parses an audio stream & attempts to convert the result into a system executable action
main :: IO ()
main = do

     args <- getArgs     
     
     res <- if null args
         then googleAudioParseSOX
         else googleAudioFileParseCurl . head $ args
     
     case res of
          Left e -> putStrLn e
          Right r -> case getResult(r) of 
                    Left e -> putStrLn e
                    Right r2 -> executeCmd r2 1 []
     
     return ()



     
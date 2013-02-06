module AudioParseResult
(
AudioParseResult(..)
)
where

import qualified Data.Text as T

class AudioParseResult a where
      getResult :: a -> Either String T.Text      
      getConfidence :: a -> Float
      getConfidence a = 0
      getAlternatives :: a -> [T.Text]
      getAlternatives a = []


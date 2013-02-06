module AudioParseResult
(
AudioParseResult(..)
)
where

import qualified Data.Text as T

class AudioParseResult a where
      getResult :: a -> Either String T.Text      
      getConfidence :: a -> Either String Float
      getConfidence a = Right(0)
      getAlternatives :: a -> Either String [T.Text]
      getAlternatives a = Right([])



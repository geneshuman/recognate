module AudioParseResult
(
AudioParseResult(..)
)
where

import qualified Data.Text as T

class AudioParseResult a where
      getResult :: a -> T.Text
      getConfidence :: a -> Maybe Float
      getAlternatives :: a -> [T.Text]


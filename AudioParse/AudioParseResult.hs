module AudioParseResult
(
AudioParseResult(..)
)
where

import qualified Data.Text as T

class AudioParseResult a where
      getResult :: a -> Maybe T.Text
      getConfidence :: a -> Maybe Float
      getAlternatives :: a -> Maybe [T.Text]


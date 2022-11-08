module Ulc.Lang.Substitution.Parser (substitutionParser) where

  import Ulc.Parser (ulcParser)
  import Ulc.Lang.Substitution.Sub (Sub(AvoidCapture))
  import Data.List.NonEmpty (fromList)
  import Text.Parsec (Parsec, between, char, letter, many1, spaces)

  substitutionParser :: Parsec String () Sub
  substitutionParser = spaces *> pSub <* spaces

  pSub :: Parsec String () Sub
  pSub = 
    do
      s <- between (char '[') (char '/') ulcParser
      f <- fmap fromList (many1 letter <* (char ']'))
      i <- ulcParser
      return $ AvoidCapture s f i
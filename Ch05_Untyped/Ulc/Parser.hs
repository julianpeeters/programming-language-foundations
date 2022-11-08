module Ulc.Parser (ulcParser) where

  import Ulc.Lang.Term (Trm(App,Lam,Var))
  import Data.List.NonEmpty (fromList)
  import Text.Parsec (Parsec, (<|>), between, char, letter, many1, spaces, try)

  ulcParser :: Parsec String () Trm
  ulcParser = spaces *> pTrm <* spaces

  pTrm :: Parsec String () Trm
  pTrm = pApp <|> pLam <|> pVar <|> pPar 

  pPar :: Parsec String () Trm
  pPar = between (char '(') (char ')') pTrm 

  pApp :: Parsec String () Trm
  pApp = fmap (\(x,y) -> App x y) (
    try(
        do
          t1 <- spaces *> pLam <|> pVar <|> pPar <* spaces
          t2 <- spaces *> pTrm <* spaces
          return (t1,t2)))

  pLam :: Parsec String () Trm
  pLam = fmap (\(x,y) -> Lam x y) (
    do
      x <- (char('λ') *> spaces *> many1 letter <* spaces)
      t <- (char('.') *> spaces *> pTrm <* spaces)
      return (fromList x,t))

  pVar :: Parsec String () Trm
  pVar = fmap (\x -> Var x) (fmap fromList (spaces *> many1 letter <* spaces))
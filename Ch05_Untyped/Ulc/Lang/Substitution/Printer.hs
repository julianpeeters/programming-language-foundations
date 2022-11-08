module Ulc.Lang.Substitution.Printer where

  import Ulc.Lang.Substitution.Sub (Sub(AvoidCapture))
  import Ulc.Printer ()
  import Data.List.NonEmpty (toList)

  instance Show Sub where
    show (AvoidCapture sub for into) =
      "[" ++
      show sub ++
      "/" ++
      (filter (/= '"') . show . toList)for ++
      "]" ++
      show into


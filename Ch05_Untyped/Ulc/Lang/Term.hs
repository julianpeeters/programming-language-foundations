module Ulc.Lang.Term (Trm(App,Lam,Var)) where

  import Data.List.NonEmpty

  data Trm =
    App Trm Trm
      | Lam (NonEmpty Char) Trm
      | Var (NonEmpty Char)

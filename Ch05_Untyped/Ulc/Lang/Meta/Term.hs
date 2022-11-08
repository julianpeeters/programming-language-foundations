module Ulc.Lang.Meta.Term (MetaTrm(MetaApp,MetaLam,MetaVar)) where

  import Data.List.NonEmpty

  data MetaTrm =
    MetaTerm Int
      | MetaApp MetaTrm MetaTrm
      | MetaLam (NonEmpty Char) MetaTrm
      | MetaVar (NonEmpty Char)
      deriving Eq
module Ulc.Lang.Substitution.Free (fv) where

  import Ulc.Lang.Term (Trm(App,Lam,Var))
  import Data.List.NonEmpty (NonEmpty)
  import Data.Set (Set, difference, singleton, union)

  fv :: Trm -> Set (NonEmpty Char)
  fv (App a b) = union (fv a) (fv b) 
  fv (Lam a b) = difference (fv b) (singleton a)
  fv (Var a) = singleton a
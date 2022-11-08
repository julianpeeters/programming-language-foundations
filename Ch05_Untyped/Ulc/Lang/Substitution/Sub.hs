module Ulc.Lang.Substitution.Sub (Sub(AvoidCapture), evalSub, substitute) where

  import Ulc.Lang.Term (Trm(App,Lam,Var))
  import Data.List.NonEmpty (NonEmpty)

  data Sub = AvoidCapture Trm (NonEmpty Char) Trm

  evalSub :: Sub -> Trm
  evalSub (AvoidCapture sub for into) = substitute sub for (into)

  substitute :: Trm -> NonEmpty Char -> Trm -> Trm
  substitute new for (App e1 e2) = App (substitute new for e1) (substitute new for e2)
  substitute new for (Lam x e) = Lam (x) (substitute new for e)
  substitute new for (Var n) = if n == for then new else Var n

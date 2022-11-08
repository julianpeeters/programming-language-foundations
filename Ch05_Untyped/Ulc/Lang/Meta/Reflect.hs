module Ulc.Lang.Meta.Reflect (reflect) where

  import Ulc.Lang.Meta.Term (MetaTrm(MetaApp,MetaLam,MetaVar))
  import Ulc.Lang.Term (Trm(App,Lam,Var))

  reflect :: Trm -> MetaTrm
  reflect (App a b) = MetaApp (reflect a) (reflect b)
  reflect (Lam a b) = MetaLam a (reflect b)
  reflect (Var a) = MetaVar a
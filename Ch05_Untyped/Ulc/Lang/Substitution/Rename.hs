module Ulc.Lang.Substitution.Rename (rename) where

  import Ulc.Lang.Substitution.Free (fv)
  import Ulc.Lang.Substitution.Sub (substitute)
  import Ulc.Lang.Term (Trm(App,Lam,Var))
  import Control.Monad.State (State, evalState, gets, put)
  import Data.List.NonEmpty (NonEmpty, fromList)
  import Data.Set (toList)

  fresh :: Trm -> State ([NonEmpty Char],[NonEmpty Char]) (NonEmpty Char)
  fresh body =
    do
      (xs,c) <- gets id
      let rem = filter (not . \x -> any ( == x) (toList (fv body))) xs
      if rem == [] then error "0 single-character variables remaining."
      else put (tail rem, c) >> return (head rem)

  rename :: Trm -> Trm
  rename t = evalState (go t) (vars, [])
    where
      vars =
        fmap (fromList . filter (/= '\'') . show)
          "xyzabcdefghijklmnopqrstuvw"
      go (App a b) =
        do
          u <- go a
          v <- go b
          return $ App u v
      go (Lam a b) =
        do
          (xs,c) <- gets id
          if any ( == a) c
          then do
            x <- fresh b
            u <- go (substitute (Var x) a b)
            return $ Lam x u
          else return $ Lam a b  
      go (Var a) =
        do
          (xs,c) <- gets id
          put (filter (not . ( == a)) xs, a:c)
          return $ Var a
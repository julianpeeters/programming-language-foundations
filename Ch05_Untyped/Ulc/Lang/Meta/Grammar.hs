module Ulc.Lang.Meta.Grammar (generalize) where

  import Ulc.Lang.Meta.Term (MetaTrm(MetaApp,MetaLam,MetaVar))
  import Control.Monad.State (State, evalState, gets, put)
  import Data.List (find)
  import Data.List.NonEmpty (NonEmpty, fromList)
  
  data Ctx =
    Ctx
      [(NonEmpty Char, NonEmpty Char)]
      [(NonEmpty Char, NonEmpty Char)]
  
  freshX :: State ([NonEmpty Char], Int, Ctx) (NonEmpty Char)
  freshX =
    do
      (xs,t,c) <- gets id
      if xs == [] then error "0 single-character meta variables remaining."
      else put (Prelude.tail xs, t, c) >> return (Prelude.head xs)

  freshT :: State ([NonEmpty Char], Int, Ctx) Int
  freshT = 
    do
      (xs,t,c) <- gets id
      put (xs,t+1,c)
      return t  

  generalize :: MetaTrm -> MetaTrm -> MetaTrm
  generalize e1 e2 = evalState (go e1 e2) (vars, 0, Ctx [] [])
    where
      vars =
        fmap (fromList . filter (/= '\'') . show)
          "xyzabcdefghijklmnopqrstuvw"
      go (MetaApp a b) (MetaApp c d) =
        do
          r1 <- go a c
          r2 <- go b d
          return $ MetaApp r1 r2
      go (MetaApp a b) (MetaLam c d) =
        fmap (\i -> MetaVar $ fromList ("t" ++ replicate i '\'')) freshT
      go (MetaApp a b) (MetaVar c) =
        fmap (\i -> MetaVar $ fromList ("t" ++ replicate i '\'')) freshT
      go (MetaLam a b) (MetaApp c d) =
        fmap (\i -> MetaVar $ fromList ("t" ++ replicate i '\'')) freshT
      go (MetaLam a b) (MetaLam c d) =
        do
          (xs,t,Ctx lc rc) <- gets id
          fx <- freshX
          put (xs,t,Ctx((a,fx):lc) ((c,fx):rc))
          ft <- go b d
          return $ MetaLam fx ft
      go (MetaLam a b) (MetaVar c) =
        fmap (\i -> MetaVar $ fromList ("t" ++ replicate i '\'')) freshT
      go (MetaVar a) (MetaApp b c) =
        fmap (\i -> MetaVar $ fromList ("t" ++ replicate i '\'')) freshT
      go (MetaVar a) (MetaLam b c) =
        fmap (\i -> MetaVar $ fromList ("t" ++ replicate i '\'')) freshT
      go (MetaVar a) (MetaVar b) =
        do
          (xs,t,Ctx lc rc) <- gets id
          case (find (( == a) . fst) lc, find (( == b) . fst) rc) of
            (Nothing,Nothing) -> fmap MetaVar freshX
            (Nothing,Just n)  ->
              fmap (\i -> MetaVar $ fromList ("t" ++ replicate i '\'')) freshT
            (Just m, Nothing) ->
              fmap (\i -> MetaVar $ fromList ("t" ++ replicate i '\'')) freshT
            (Just m, Just n)  ->
              if (snd m) == (snd n)
              then return $ MetaVar $ snd m
              else fmap (\i -> MetaVar $ fromList ("t" ++ replicate i '\'')) freshT
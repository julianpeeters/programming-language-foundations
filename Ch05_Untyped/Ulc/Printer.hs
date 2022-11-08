module Ulc.Printer (renderFreeVars, renderFullParens, renderTree) where

  import Ulc.Lang.Meta.Term (MetaTrm(MetaApp,MetaLam,MetaVar))
  import Ulc.Lang.Term (Trm(App,Lam,Var))
  import Data.List.NonEmpty (NonEmpty, toList)
  import Data.Set (Set, toList)

  instance Show Trm where
    show (App a b) =
      show a ++
      " " ++
      show b
    show (Lam a b) = "(" ++
      "λ" ++ (filter (/= '"') . show . Data.List.NonEmpty.toList) a ++
      "." ++ show b ++ ")"
    show (Var a) =
      (filter (/= '"') . show . Data.List.NonEmpty.toList) a

  instance Show MetaTrm where
    show (MetaApp a b) = show a ++ " " ++ show b
    show (MetaLam a b) =
      "(" ++
      "λ" ++ (filter (/= '"') . show . Data.List.NonEmpty.toList) a ++
      "." ++ show b ++
      ")"
    show (MetaVar a) = (filter (/= '"') . show . Data.List.NonEmpty.toList) a

  renderFullParens :: Trm -> String
  renderFullParens (App a b) =
    "(" ++
    "(" ++ renderFullParens a ++ ")" ++
    " " ++
    "(" ++ renderFullParens b ++ ")" ++
    ")"
  renderFullParens (Lam a b) =
    "(" ++
    "λ" ++ (filter (/= '"') . show . Data.List.NonEmpty.toList) a ++
    "." ++ "(" ++ renderFullParens b ++ ")" ++
    ")"
  renderFullParens (Var a) =
    (filter (/= '"') . show . Data.List.NonEmpty.toList) a

  renderTree :: Trm -> String
  renderTree trm ="    " ++ go 1 trm
    where
      go level (App a b) =
        "@\n" ++
        (concat $ replicate level "  ") ++ " / \\\n" ++ (concat $ replicate level "  ") ++
        go (level + 1) a ++
        "   " ++
        go (level + 1) b
      go level (Lam a b) =
        "λ\n" ++
        (concat $ replicate level "  ") ++ " / \\\n" ++ (concat $ replicate level "  ") ++
        (filter (/= '"') . show . Data.List.NonEmpty.toList) a ++
        "   " ++
        go (level + 1) b
      go level (Var a) =
        (filter (/= '"') . show . Data.List.NonEmpty.toList) a

  renderFreeVars :: Set (NonEmpty Char) -> String
  renderFreeVars fv = (
    (filter (/= '[')) .
    (filter (/= ']')) .
    (filter (/= '"')) .
    show . (fmap (Data.List.NonEmpty.toList)) . Data.Set.toList)
      fv
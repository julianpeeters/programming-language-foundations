module UlcExercises (ulcExercises) where

  import Ulc.Lang.Meta.Grammar (generalize)
  import Ulc.Lang.Meta.Reflect (reflect)
  import Ulc.Lang.Meta.Term (MetaTrm)
  import Ulc.Parser (ulcParser)
  import Ulc.Printer (renderFreeVars, renderFullParens, renderTree)
  import Ulc.Lang.Substitution.Free (fv)
  import Ulc.Lang.Substitution.Parser (substitutionParser)
  import Ulc.Lang.Substitution.Printer ()
  import Ulc.Lang.Substitution.Rename (rename)
  import Ulc.Lang.Substitution.Sub (evalSub, substitute)
  import Ulc.Lang.Term (Trm)
  import Data.Foldable (traverse_)
  import Data.List.NonEmpty (toList)
  import Text.Parsec (ParseError, parse)
    
  ulcExercises :: (String -> IO ()) -> (Either ParseError String -> String) -> IO ()
  ulcExercises print fold = 
    do
      traverse_ (print . fold) [
        f "5.8.1-1a. Syntax tree" renderTree "λx.λy.(x y)",
        f "5.8.1-1b. Syntax tree" renderTree "λx.x (λy.y y)",
        f "5.8.1-1c. Syntax tree" renderTree "x λx.x y x",
        g "5.8.1-2. Meta-level expr" show "λx.(λy.y) (x x)" "λx.x (x λy.y)",
        f "5.8.1-3. An example lambda term of three used binders" show "λx.λy.λz.x y z",
        f "5.8.1-4a. Fully parenthesized rendering" renderFullParens "λx.λy.x",
        f "5.8.1-4b. Fully parenthesized rendering" renderFullParens "x x x",
        f "5.8.1-4c. Fully parenthesized rendering" renderFullParens "x λx.x x",
        f "5.8.1-5a. Minimally parenthesized rendering" show "((λx.(x x)) x)",
        f "5.8.1-5b. Minimally parenthesized rendering" show "((λy.y) (λx.(x (x x))))",
        f "5.8.1-5c. Minimally parenthesized rendering" show "((λx.(λy.((x y) x))) z)",
        f "5.8.1-6a. Renamed local variables" (show . rename) "x y λx.λy.z",
        f "5.8.1-6b. Renamed local variables" (show . rename) "(λx.x x) (λx.x x)",
        f "5.8.1-6c. Renamed local variables" (show . rename) "(λx.x) y (λx.x y)",
        f "5.8.2-1a. Free variables" (renderFreeVars . fv) "x y λx.x y",
        f "5.8.2-1b. Free variables" (renderFreeVars . fv) "λx.y x x",
        f "5.8.2-1c. Free variables" (renderFreeVars . fv) "λx.(λy.y) y λx.x",
        h "5.8.2-2a. Substitution" (show . evalSub) "[x/y](λz.z y)",
        h "5.8.2-2b. Substitution" (show . evalSub) "[(x x)/x](λz.x y z)",
        h "5.8.2-2c. Substitution" (show . evalSub) "[(z x)/x](λz.x z)"]

    where
      f l r t =
        fmap (\x-> l ++ " of: "++ show x ++ " =\n"++ r x ++ "\n") (parse ulcParser "" t)
      g l r t1 t2 =
        do
          e1 <- fmap reflect (parse ulcParser "" t1)
          e2 <- fmap reflect (parse ulcParser "" t2)
          Right(l ++ " of: "++ t1 ++ " and " ++ t2 ++ " =\n" ++ r (generalize e1 e2) ++ "\n")
      h l r t =
        fmap (\x-> l ++ " of: "++ show x ++ " =\n"++ r x ++ "\n") (parse substitutionParser "" t)
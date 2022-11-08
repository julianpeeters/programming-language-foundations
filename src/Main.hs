import UlcExercises (ulcExercises)
import Ulc.Parser (ulcParser)
import Control.Exception
import System.IO
import Text.Parsec

test :: Show a => (Parsec String () a) -> String -> IO ()
test parser path = bracket (openFile path ReadMode) hClose $
  \h -> hGetContents h >>= parseTest parser

run :: ((String -> IO ()) -> (Either ParseError String -> String) -> IO ())
    -> IO ()
run exercises = exercises putStrLn (foldr (\x y -> x) (error "bang!"))

main :: IO ()
main =
  do
    test ulcParser "Ch05_Untyped/Example.ulc"
    run ulcExercises
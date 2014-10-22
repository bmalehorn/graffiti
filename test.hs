import qualified Graffiti
import System.IO

main = do
  case Graffiti.test of
    Right () -> putStrLn "passed all tests"
    Left s -> hPutStrLn stderr s

import Control.Monad
import Data.Char
import Flow

main :: IO ()
main = helper []

helper :: [[Char]] -> IO ()
helper ls = do
  line <- getLine
  if null line then
    putStrLn <| unlines ls
  else do
    helper (line : ls)

-- import System.IO
import System.Environment
import Data.Map as M
import Data.List as L
import Control.Exception
import Control.Monad
import Flow
import System.Exit
import System.IO (hPutStrLn, stderr)

data Result = Ok | Problem String

padString :: String -> Int -> String -> String
padString padding width soFar =
  if length soFar >= width then
    soFar
  else
    padString padding width <| padding ++ soFar

prependNumber :: Int -> Int -> String -> String
prependNumber width n s =
  (show n |> padString " " width) ++ "│ " ++ s

numDigits :: (RealFloat a) => (RealFloat a) => a -> a -> Int
numDigits base =
  logBase base .> floor .> succ

printFile :: (String, FilePath) -> IO Result
printFile (idx, path) = do
  contentsOrExcept <- try <| readFile path
  case contentsOrExcept of
    Left except -> do
      return <| Problem <| "Could not print file " ++ path ++ "\n  instead got error: " ++ (show (except :: IOException))
    Right contents ->
      let
        ls = lines contents
        lineNumsWidth = (length ls |> fromIntegral |> numDigits 10)
        numFence = replicate lineNumsWidth '─'
        maxLineWidth = L.map length ls |> maximum |> succ
        path' = idx ++ path
        pathWidth = length path'
        lineFence = rep '─' maxLineWidth
        tweenFence = rep '─' (pathWidth - lineNumsWidth - 1)
      in do
        putStr <| unlines
          [ "╭" ++ rep '─' pathWidth ++ "╮"
          , "│" ++ path' ++ "│"
          , "├" ++ numFence ++ "┬" ++ tweenFence ++ "┴" ++ rep '─' (maxLineWidth - pathWidth + lineNumsWidth) ++ "╮"
          ]
        _ <- ls
          |> humanIndexedMap (prependNumber lineNumsWidth)
          |> mapM (
            padEndString " " (lineNumsWidth + maxLineWidth + 1)
            .> (++"│")
            .> ('│':)
            .> putStrLn
          )
        when (last ls /= "") <| putStrLn <| concat <|
          [ '│' : padString " " (lineNumsWidth - 1) "❌"
          , padEndString " " (maxLineWidth + 1) "│ No New Line At End Of File"
          , "│"
          ]
        putStrLn <| "╰" ++ numFence ++ "┴" ++ lineFence ++ "╯"
        return Ok

isProblematic :: Result -> Bool
isProblematic Ok = False
isProblematic (Problem _) = True

resultToString :: Result -> String
resultToString Ok = ""
resultToString (Problem s) = s

humanIndexedMap :: (Int -> a -> b) -> [a] -> [b]
humanIndexedMap fn =
  zipWith fn [1..]

resultToStatusMark :: Result -> Char
resultToStatusMark Ok = '✅'
resultToStatusMark (Problem _) = '❌'

padEndString :: String -> Int -> String -> String
padEndString padding width =
  reverse
  .> padString padding width
  .> reverse

appendStatus :: Int -> Result -> String -> String
appendStatus width r =
  padEndString " " width
  .> (++('│' : (resultToStatusMark r) : "" ))

errLn :: String -> IO ()
errLn = hPutStrLn stderr

problemSummaries :: [Result] -> [String]
problemSummaries [] = []
problemSummaries (Ok : rest) = problemSummaries rest
problemSummaries (Problem s : rest) = s : problemSummaries rest

printFiles :: [FilePath] -> IO ()
printFiles paths =
  let
    numPaths = length paths
    pathsNumWidth = (numPaths |> fromIntegral |> numDigits 10)
    indices = if numPaths > 1
      then
        [ show n |> padString " " pathsNumWidth |> (++" ") | n <- [1..] ]
      else
        repeat ""
    pathsWithIndices = zip indices paths
  in do
  results <- mapM printFile pathsWithIndices
  case problemSummaries results of
    [] ->
      exitSuccess
    problems ->
      let
        numFence = replicate pathsNumWidth '─'
        maxArgWidth = L.map length paths |> maximum |> succ
        argFence = rep '─' maxArgWidth
      in do
        _ <- errLn <| "╭" ++ numFence ++ "┬" ++ argFence ++ "╮"
        _ <- paths
          |> humanIndexedMap (prependNumber pathsNumWidth)
          |> zipWith (appendStatus (pathsNumWidth + maxArgWidth + 1)) results
          |> mapM (
            ('│':)
            .> errLn
          )
        errLn <| "╰" ++ numFence ++ "┴" ++ argFence ++ "╯"
        die <| unlines problems

rep :: a -> Int -> [a]
rep a n =
  repeat a |> L.take n

main :: IO ()
main = do
  args <- getArgs
  printFiles args

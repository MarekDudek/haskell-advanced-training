module Main where

import System.Environment
import System.Console.GetOpt

import Data.List

main = do
  putStrLn "Haskell advanced training"
  args        <- getArgs
  programName <- getProgName
  let ( opts, nonOpts, msgs ) = getOpt Permute options args
  case (opts, nonOpts, msgs ) of 
    (opts, [],      []  ) -> do 
      putStr $ "Options: "
      let optionString = map show opts
      mapM_ putStr (intersperse ", " optionString)
      putStrLn ""
    (_,     nonOpts, msgs) -> do 
      putStrLn $ "Unrecognised arguments: " ++ unwords nonOpts
      putStrLn "Error messages: " 
      mapM_ putStrLn msgs
      error $ usageInfo (header programName) options
  

options :: [OptDescr Flag]
options = [versionOption, inputOption, outputOption]

header name = "Usage: " ++ name ++ " [OPTION ...]"
data Flag = Version | Input String | Output (Maybe String)
  deriving Show

versionOption = Option ['v'] ["version"] (NoArg Version)        "show version number"
inputOption   = Option ['i'] ["input"]   (ReqArg Input  "FILE") "input file"
outputOption  = Option ['o'] ["output"]  (OptArg Output "FILE") "output file"

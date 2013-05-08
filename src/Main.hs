module Main where

import System.Environment
import System.Console.GetOpt


main = do
  putStrLn "Haskell advanced training"

  args <- getArgs
  -- putStrLn $ "Arguments: " ++ show args

  programName <- getProgName
  -- putStrLn $ "Program name is " ++ programName

  let ( flags, nonOpts, msgs ) = getOpt RequireOrder options args
  -- debugCommandLine flags nonOpts msgs
  case (flags, nonOpts, msgs ) of 
    (flags, [],      []  ) -> 
      do 
        putStr $ "All OK: "
        mapM_ putStr (map show flags)
        putStrLn ""
    (_,     nonOpts, []  ) -> 
      do
        putStrLn $ "Unrecognised arguments: " ++ unwords nonOpts
        error $ usageInfo header options
    (_,     nonOpts, msgs) -> 
      do 
        putStrLn $ "Unrecognised arguments: " ++ unwords nonOpts
        putStrLn "Error messages: " 
        mapM_ putStr msgs
        error $ usageInfo header options
  

options :: [OptDescr Flag]
options = [versionOption]

header = "Usage: hat [OPTION ...]"
data Flag = Version deriving Show

versionOption = Option ['v'] ["version"] (NoArg Version) "show version number"
  
debugCommandLine flags nonOpts msgs = do
  putStrLn $ "#flags  : " ++ show (length flags)
  putStrLn $ "flags   : " ++ show flags
  putStrLn $ "non-opts: " ++ show nonOpts
  putStrLn $ "messages: " ++ show msgs


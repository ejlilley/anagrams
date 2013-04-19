module Main where

import Anagrams
import System.IO
import Control.Monad
import System.Environment
import System.Exit
import System.Console.GetOpt    

data Flag
        = File String
        | Help
        deriving (Eq,Ord,Show)

flags =
       [Option ['f'] []       (ReqArg File "FILE") "Choose the dictionary file.",
        Option []    ["help"] (NoArg Help) "Print this help message."
       ]

parse argv = case getOpt Permute flags argv of
        (args,fs,[]) -> do
            let files = if null fs then ["-"] else fs
            if Help `elem` args
                then do hPutStrLn stderr (usageInfo header flags)
                        exitWith ExitSuccess
--                 else return (uniq (concatMap set args), files)
                else return (args, files)
        (_,_,errs)      -> do
            hPutStrLn stderr (concat errs ++ usageInfo header flags)
            exitWith (ExitFailure 1)
        where header = "Usage: anagrams [-f file] [string or phrase]"

parseFile :: Flag -> String
parseFile (File s) = s
parseFile Help = ""

getFile :: [Flag] -> String
getFile flags = let result = foldl (++) "" (map parseFile flags)
                in if result == ""
                   then "/usr/share/dict/words"
                   else result

illegal = ['0'..'9'] ++ ['&']
caps = ['A'..'Z']

member [] _ = False
member (x:xs) y = x == y || member xs y

filterLength n = filter (\x -> length x >= n)
filterChars = filter (\x -> not $ or $ map (member x) illegal)
enforceChars goal = filter (\x -> or $ map (member (normalise x)) (normalise goal))
filterAllCaps = filter (\x -> not $ and $ map (member caps) x)
filterPropers = filter (\x -> not $ member caps (head x))

main :: IO ()
main = do
  (as, phrase) <- getArgs >>= parse
  let f = getFile as
  handle <- openFile f ReadMode
  contents <- hGetContents handle

  let allwords = lines contents
      goalstring = joinS "" phrase
      filterAll = filterChars 
                  . (enforceChars goalstring) 
                  . filterAllCaps 
                  . filterPropers
                  . (filterLength 3)
      dict = filterAll allwords

  putStrLn ("Using " ++ f ++ " as dictionary.")
  
  putStrLn ("Dictionary contains " ++ (show (length allwords)) ++ " words in total.")

  let goalanag = makeAnag goalstring
      processedDict = map makeAnag dict
      subsetDict = filter (\x -> (fst x) `subsetVector` (fst goalanag)) processedDict
  
  putStrLn ("Using " ++ (show (length subsetDict)) ++ " of them.")
  
  putStrLn ("Input string is \"" ++ goalstring ++ "\".")

  let result = searchAnag goalanag subsetDict
      results = map (joinS " ") result
      resultstring = if results == []
                     then "No results. ☹"
                     else joinS "\n" results

  putStrLn resultstring
  hClose handle  
  exitWith ExitSuccess

module Main where

import Editor
import System.IO

-- ==== config ==== --

width :: Int
width = 80

-- ==== main ==== --

main :: IO ()
main =
  noBuffering $
  do messagebox [ "Welcome to funOmega version 0.0.1"
                , ""
                , " - In this interactive environment, you can type in terms"
                , "   to evaluate them."
                , ""
                , " - For additional help, type in ':?'"
                , ""
                ]
     repl

repl :: IO ()
repl =
  do x <- runLineEditor "funOmega> " $ takeInput >>= remember
     putStrLn $ "your code should do something with '" ++ x ++ "' here {^o^}"
     repl

-- ==== util ==== --

newline :: IO ()
newline = putStr ""

initline  :: IO ()
initline =
  do putStr "*"
     putStrLn $ replicate (width - 1) '='

endline :: IO ()
endline =
  do putStr $ replicate (width - 1) '='
     putStrLn "*"

putBoxLine :: String -> IO ()
putBoxLine x =
  do putStr "| "
     putStr x
     putStr $ replicate ((width - 4 ) - length x) ' '
     putStrLn " |"

messagebox :: [String] -> IO ()
messagebox xs =
  do initline
     mapM_ putBoxLine xs
     endline

noBuffering :: IO a -> IO a
noBuffering op =
  do stdinEcho        <- hGetEcho stdin
     stdinBufferMode  <- hGetBuffering stdin
     stdoutBufferMode <- hGetBuffering stdout
     hSetEcho      stdin  False
     hSetBuffering stdin  NoBuffering
     hSetBuffering stdout NoBuffering
     a <- op
     hSetEcho      stdin  stdinEcho
     hSetBuffering stdin  stdinBufferMode
     hSetBuffering stdout stdoutBufferMode
     return a

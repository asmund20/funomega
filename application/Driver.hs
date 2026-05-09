{-# LANGUAGE RecordWildCards #-}

module Main (main) where

import Control.Monad (forM)
import Control.Monad.State
import Data.List (intercalate)
import Editor
import Interpreter
import Parser
import Syntax
import System.Exit (exitSuccess)
import System.IO
import TypeChecker

data ReplState = ReplState
    { rsFiles :: [FilePath]
    , rsOpTable :: OpTable
    , rsProgram :: Program
    }

type ReplM = StateT ReplState IO

-- TODO: Make the files a list, or check that the new file is not in there.
addFile :: FilePath -> ReplM ()
addFile f = do
    files <- rsFiles <$> get
    modify' $ \st -> st{rsFiles = f : files}

setTable :: OpTable -> ReplM ()
setTable t = modify' $ \st -> st{rsOpTable = t}

setProgram :: Program -> ReplM ()
setProgram p = modify' $ \st -> st{rsProgram = p}

-- ==== config ==== --

width :: Int
width = 80

-- ==== main ==== --

main :: IO ()
main =
    noBuffering $ do
        messagebox
            [ "Welcome to funOmega version 0.0.1"
            , ""
            , " - In this interactive environment, you can type in terms"
            , "   to evaluate them."
            , ""
            , " - For additional help, type in ':?'"
            , ""
            ]
        evalStateT
            repl
            ReplState
                { rsFiles = []
                , rsOpTable = emptyOpTable
                , rsProgram = Program (const Nothing) (const Nothing)
                }

repl :: ReplM ()
repl = do
    x <- lift $ runLineEditor "funOmega> " $ takeInput >>= remember
    table <- rsOpTable <$> get
    case parseCommand table x of
        Left e -> lift $ putStrLn $ show e
        Right Quit -> lift exitSuccess
        Right c -> do
            case c of
                Load f -> do
                    addFile f
                    loadFiles
                Reload -> loadFiles
                Eval t -> do
                    prog <- rsProgram <$> get
                    case runTermCheck t prog of
                        Left e -> lift $ putStrLn $ show e
                        Right _ ->
                            case interpretTerm prog t of
                                Left e -> lift $ putStrLn $ show e
                                Right v -> lift $ putStrLn $ prettyValue v
                Help -> lift printHelp
    repl

printHelp :: IO ()
printHelp =
    messagebox
        [ "You can either run a command or type in a funOmega term here."
        , "Commands:"
        , "- :? ==> Print this help message"
        , "- :q ==> Quit the repl"
        , "- :l <path> ==> Load the definitions in a funOmega file"
        , "- :r ==> Reload the loaded funOmega files"
        ]

loadFiles :: ReplM ()
loadFiles = do
    files <- rsFiles <$> get
    sources <- lift $ forM files readFile
    let source = intercalate "\n" sources
    case parseDefinitions source of
        Left e -> lift $ putStrLn $ show e
        Right (defs, table) -> do
            let c = checkProgram defs
            case c of
                Left e -> lift $ putStrLn $ show e
                Right prog -> do
                    setProgram prog
                    setTable table
                    lift $ messagebox $ ["Successfylly loaded files "] ++ files

-- ==== util ==== --

initline :: IO ()
initline = do
    putStr "*"
    putStrLn $ replicate (width - 1) '='

endline :: IO ()
endline = do
    putStr $ replicate (width - 1) '='
    putStrLn "*"

putBoxLine :: String -> IO ()
putBoxLine x = do
    putStr "| "
    putStr x
    putStr $ replicate ((width - 4) - length x) ' '
    putStrLn " |"

messagebox :: [String] -> IO ()
messagebox xs = do
    initline
    mapM_ putBoxLine xs
    endline

noBuffering :: IO a -> IO a
noBuffering op = do
    stdinEcho <- hGetEcho stdin
    stdinBufferMode <- hGetBuffering stdin
    stdoutBufferMode <- hGetBuffering stdout
    hSetEcho stdin False
    hSetBuffering stdin NoBuffering
    hSetBuffering stdout NoBuffering
    a <- op
    hSetEcho stdin stdinEcho
    hSetBuffering stdin stdinBufferMode
    hSetBuffering stdout stdoutBufferMode
    return a

{-# LANGUAGE RecordWildCards #-}

module Main (main, handleCommand, ErrorCase) where

import Control.Monad (forM)
import Control.Monad.State
import Data.List (intercalate)
import Data.Set (Set)
import qualified Data.Set as Set
import Editor
import Interpreter
import Parser
import Syntax
import System.Directory (doesFileExist)
import System.Exit (exitSuccess)
import System.IO
import TypeChecker

data ReplState = ReplState
    { rsFiles :: Set FilePath
    , rsOpTable :: OpTable
    , rsProgram :: Program
    }

type ReplM = StateT ReplState IO

data ErrorCase
    = CommandParseError
    | FileNotFoundError
    | TypeCheckError
    | RunTimeError
    | ProgramParseError

addFile :: FilePath -> ReplM Bool
addFile f = do
    exists <- lift $ doesFileExist f
    if exists
        then do
            files <- rsFiles <$> get
            modify' $ \st -> st{rsFiles = Set.insert f files}
            return True
        else do
            return False

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
                { rsFiles = Set.empty
                , rsOpTable = emptyOpTable
                , rsProgram = Program (const Nothing) (const Nothing) (const False)
                }

repl :: ReplM ()
repl = do
    x <- lift $ runLineEditor "funOmega> " $ takeInput >>= remember
    _ <- handleCommand x
    repl

handleCommand :: String -> ReplM (Maybe ErrorCase)
handleCommand x = do
    table <- rsOpTable <$> get
    case parseCommand table x of
        Left e -> do
            lift $ putStrLn $ show e
            return $ Just CommandParseError
        Right Quit -> lift exitSuccess
        Right c -> do
            case c of
                Load f -> do
                    exists <- addFile f
                    if exists
                        then do
                            loadFiles
                        else do
                            lift $ putStrLn $ "Cannot find file " ++ f
                            return $ Just FileNotFoundError
                Reload -> do
                    loadFiles
                Eval t -> do
                    prog <- rsProgram <$> get
                    case runTermCheck t prog of
                        Left e -> do
                            lift $ putStrLn $ show e
                            return $ Just TypeCheckError
                        Right _ ->
                            case interpretTerm prog t of
                                Left e -> do
                                    lift $ putStrLn $ show e
                                    return $ Just RunTimeError
                                Right v -> do
                                    lift $ putStrLn $ prettyValue v
                                    return Nothing
                Help -> do
                    lift printHelp
                    return Nothing

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

loadFiles :: ReplM (Maybe ErrorCase)
loadFiles = do
    files <- Set.toList <$> rsFiles <$> get
    sources <- lift $ forM files readFile
    let source = intercalate "\n" sources
    case parseDefinitions source of
        Left e -> do
            lift $ putStrLn $ show e
            return $ Just ProgramParseError
        Right (defs, table) -> do
            let c = checkProgram defs
            case c of
                Left e -> do
                    lift $ putStrLn $ show e
                    return $ Just TypeCheckError
                Right prog -> do
                    setProgram prog
                    setTable table
                    lift $ putStrLn $ show defs
                    lift $ messagebox $ ["Successfylly loaded files "] ++ files
                    return Nothing

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

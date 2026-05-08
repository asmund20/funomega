{-# LANGUAGE DeriveFunctor #-}

module Editor (LineEdit, runLineEditor, takeInput, remember)
where

-- A simple minded line editor.

import Control.Monad.Free
import System.Exit (exitSuccess)
import System.IO
import System.IO.Error (catchIOError)

-- ==== config ==== --

-- The editor leaves a file where it stores the inputs to `remember`. This
-- is a pragmatic choice which may not persist in future versions of this
-- software.

history_path :: FilePath
history_path = "funOmegaHistory"

-- ==== domain ==== --

-- A cursor, is represented by two lists (the stuff before/after).
--
--   The configuration (things, []) means that the cursor is all the way at
--   the end, and ([], things) means that it is all the way ahead.

type Line = String
type Prompt = String
type Cursor a = ([a], [a])
type Editor = Cursor Char
type History = Cursor Editor

-- The empty cursor is defined by two empty lists.

empty :: Cursor a
empty = (mempty, mempty)

-- Removing the cursor from a sequence, just yields the sequence.
--
--   This particular algorithm is faster when the cursor is close to the end
--   (which I assume it will be more often than not).

uncursor :: Cursor a -> [a]
uncursor (as, es) = reverse (reverse es <> as)

-- ==== IO operations == ----

loadHistory :: IO History
loadHistory =
    catchIOError
        ( fmap (flip (,) []) $
            fmap reverse $
                fmap (fmap $ flip (,) []) $
                    fmap (fmap $ reverse) $
                        fmap lines $
                            readFile' history_path
        )
        ( const $
            do
                openFile history_path WriteMode >>= hClose
                loadHistory
        )

appendHistory :: Line -> IO ()
appendHistory l =
    do
        h <- loadHistory
        let h' = map uncursor (uncursor h) ++ [l]
        writeFile history_path $ unlines h'

-- ==== monad ==== --

-- In future work, this set of operations will include stuff like auto
-- completion.
data EditOperation a
    = TakeInput (Line -> a)
    | Remember Line a
    deriving (Functor)

type LineEdit = Free EditOperation

runLineEditor :: Prompt -> LineEdit a -> IO a
runLineEditor prompt m' =
    do
        h' <- loadHistory
        show' empty
        runLineEditor' h' empty m'
  where
    getCharSafe = catchIOError getChar (const $ pure '\n')
    clearline = putStr "\ESC[2K\r"
    output e1 = show' e1 >> hFlush stdout
    show' :: Editor -> IO ()
    show' (as, bs) =
        do
            clearline
            hFlush stdout
            putStr prompt
            putStr $ reverse as
            saveCursorPosition
            putStr bs
            restoreCursorPosition
    runLineEditor' :: History -> Editor -> LineEdit a -> IO a
    runLineEditor' h c@(hc, tc) m =
        do
            case m of
                Pure a -> return a
                Free fa ->
                    case fa of
                        Remember l a ->
                            do
                                appendHistory l
                                h'' <- loadHistory
                                runLineEditor' h'' c a
                        TakeInput f ->
                            do
                                char <- getCharSafe
                                case char of
                                    -- Exit on EOT
                                    '\EOT' -> exitSuccess
                                    -- return on newline.
                                    '\n' ->
                                        do
                                            newline
                                            runLineEditor' h c (f $ reverse hc ++ tc)
                                    -- backspace
                                    '\DEL' ->
                                        case hc of
                                            [] ->
                                                do runLineEditor' h c m
                                            (_ : hc') ->
                                                do
                                                    output (hc', tc)
                                                    runLineEditor' h (hc', tc) m
                                    -- escape characters
                                    '\ESC' ->
                                        do
                                            next <- getCharSafe
                                            case next of
                                                '[' ->
                                                    do
                                                        direction <- getCharSafe
                                                        case direction of
                                                            -- up arrow
                                                            'A' ->
                                                                case h of
                                                                    ([], _) -> continue
                                                                    (e : h0, h1) ->
                                                                        do
                                                                            output e
                                                                            runLineEditor' (h0, c : h1) e m
                                                            -- down arrow
                                                            'B' ->
                                                                case h of
                                                                    (_, []) -> continue
                                                                    (h0, e : h1) ->
                                                                        do
                                                                            output e
                                                                            runLineEditor' (c : h0, h1) e m
                                                            -- right arrow
                                                            'C' ->
                                                                case tc of
                                                                    [] -> continue
                                                                    (a : tc') ->
                                                                        do
                                                                            output (a : hc, tc')
                                                                            runLineEditor' h (a : hc, tc') m
                                                            -- left arrow
                                                            'D' ->
                                                                case hc of
                                                                    [] -> continue
                                                                    (a : hc') ->
                                                                        do
                                                                            output (hc', a : tc)
                                                                            runLineEditor' h (hc', a : tc) m
                                                            -- Probably the Delete key
                                                            '3' ->
                                                                do
                                                                    next' <- getCharSafe
                                                                    case next' of
                                                                        -- Definitely the Delete key
                                                                        '~' ->
                                                                            case tc of
                                                                                [] -> continue
                                                                                (_ : tc') ->
                                                                                    do
                                                                                        output (hc, tc')
                                                                                        runLineEditor' h (hc, tc') m
                                                                        a ->
                                                                            do
                                                                                output (a : hc, tc)
                                                                                runLineEditor' h (a : hc, tc) m
                                                            a ->
                                                                do
                                                                    output (a : hc, tc)
                                                                    runLineEditor' h (a : hc, tc) m
                                                _ -> continue
                                    a -> do
                                        output (a : hc, tc)
                                        runLineEditor' h (a : hc, tc) m
      where
        continue = runLineEditor' h c m

-- ==== combinators ==== --

takeInput :: LineEdit Line
takeInput = Free $ TakeInput pure

remember :: Line -> LineEdit Line
remember l = Free $ Remember l (pure l)

-- ==== utility ==== --

saveCursorPosition :: IO ()
saveCursorPosition = putStr "\ESC[s"

restoreCursorPosition :: IO ()
restoreCursorPosition = putStr "\ESC[u"

newline :: IO ()
newline = putStrLn ""

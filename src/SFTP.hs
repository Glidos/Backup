module SFTP
    ( SFTP,
    withSFTP,
    listDirectory,
    upload,
    download,
    deleteFile
    ) where

import System.IO                      (Handle, hPutStrLn, hGetChar, hGetLine, hFlush, hClose)
import System.Process.Typed           (Process, getStdin, getStdout, setStdin, setStdout, setStderr, closed, createPipe, shell, withProcess, checkExitCode)
import Data.List                      (isInfixOf, isPrefixOf)
import Control.Conditional            (select)
import Control.Monad.Trans.State.Lazy (StateT, evalStateT, get)
import Control.Monad.IO.Class         (liftIO)

import Util                 (dropFromEnd)

type SFTPProcess = Process Handle Handle ()
type SFTP a = StateT SFTPProcess IO a

withSFTP :: String -> SFTP a -> IO a
withSFTP url commands =
    let config = setStdin createPipe
               $ setStdout createPipe
               $ setStderr closed
               $ shell $ "unbuffer -p sftp " ++ url

    in withProcess config $ \p -> do
        hGetUptoMark (getStdout p) "sftp>"
        result <- evalStateT commands p
        hClose (getStdin p)
        checkExitCode p
        return result

cd :: String -> SFTP ()
cd path = runCommand (unwords ["cd", path]) (null . lines) (const ())

list :: SFTP [String]
list = runCommand "ls -1" (const True) (map init . lines)
-- output is a line per item, each with an extra CR char at the end

listDirectory :: String -> SFTP [String]
listDirectory path = cd path >> list


upload :: String -> String -> SFTP ()
upload lpath rpath = runCommand (unwords ["put", lpath, rpath]) (isInfixOf "100%") (const ())
-- command reports progress, getting to 100% on success


download :: String -> String -> SFTP ()
download rpath lpath = runCommand (unwords ["get", rpath, lpath]) (isInfixOf "100%") (const ())
-- command reports progress, geting to 100% on success

deleteFile :: String -> SFTP ()
deleteFile path = runCommand (unwords ["rm", path]) ((<=1) . length. lines) (const ())
-- More than 1 line implies an error message

runCommand :: String -> (String -> Bool) -> (String -> a) -> SFTP a
runCommand cmd test value =
    get >>= \p -> liftIO (hPutStrLn (getStdin p) cmd
                            >> hFlush (getStdin p)
                            >> hGetLine (getStdout p)
                            >> hGetUptoMark (getStdout p) "sftp>"
                            >>= select test (return . value) (fail . dropFromEnd 2 . last . lines))

-- Read characters from a handle until a marking string is found at the begining of a line
-- and return what has been read upto that mark.
--
-- Implment by buffering the read-in characters in reverse order.
--
-- The mark should be recognised only at the begining of a line. Since the characters
-- are buffered in reverse, the check required is that the mark appears followed by
-- either no characters or a '\n'. That can be tested by, checking for the mark,
-- dropping it and calling take 1 (which may yield an empty list) and checking that
-- all obtained characters are '\n'
hGetUptoMark :: Handle -> String -> IO String
hGetUptoMark h mark = reverse . drop len <$> f "" where
    rmark = reverse mark
    len = length mark
    f buffer =
        if rmark `isPrefixOf` buffer && all (== '\n') (take 1 $ drop len buffer)
            then return buffer
            else f . (: buffer) =<< hGetChar h


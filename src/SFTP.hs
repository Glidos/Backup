module SFTP
    ( withSFTP,
    listDirectory,
    upload,
    download,
    deleteFile
    ) where

import Util
import System.IO
import System.Process.Typed
import Data.List
import Data.Bool
import Control.Conditional

newtype SFTP = SFTP (Process Handle Handle ())

withSFTP :: String -> (SFTP -> IO a) -> IO a
withSFTP url commands =
    let config = setStdin createPipe
               $ setStdout createPipe
               $ setStderr closed
               $ shell $ "unbuffer -p sftp " ++ url

    in withProcess config $ \p -> do
        hGetUptoMark (getStdout p) "sftp>"
        result <- commands (SFTP p)
        hClose (getStdin p)
        checkExitCode p
        return result

cd :: SFTP -> String -> IO ()
cd sftp path = runCommand sftp (unwords ["cd", path]) (null . lines) (const ())

list :: SFTP -> IO [String]
list sftp = runCommand sftp "ls -1" (const True) (map init . lines)
-- output is a line per item, each with an extra CR char at the end

listDirectory :: SFTP -> String -> IO [String]
listDirectory sftp path = cd sftp path >> list sftp


upload :: SFTP -> String -> String -> IO ()
upload sftp lpath rpath = runCommand sftp (unwords ["put", lpath, rpath]) (isInfixOf "100%") (const ())
-- command reports progress, getting to 100% on success


download :: SFTP -> String -> String -> IO ()
download sftp rpath lpath = runCommand sftp (unwords ["get", rpath, lpath]) (isInfixOf "100%") (const ())
-- command reports progress, geting to 100% on success

deleteFile :: SFTP -> String -> IO ()
deleteFile sftp path = runCommand sftp (unwords ["rm", path]) ((<=1) . length. lines) (const ())
-- More than 1 line implies an error message

runCommand :: SFTP -> String -> (String -> Bool) -> (String -> a) -> IO a
runCommand (SFTP p) cmd test value =
    hPutStrLn (getStdin p) cmd
    >> hFlush (getStdin p)
    >> hGetLine (getStdout p)
    >> hGetUptoMark (getStdout p) "sftp>"
    >>= select test (return . value) (fail . dropFromEnd 2 . last . lines)

-- Read characters from a handle until a marking string is found at the begining of a line
-- and return what has been read upto that mark.
--
-- Implment by buffering the read-in characters in reverse order.
hGetUptoMark :: Handle -> String -> IO String
hGetUptoMark h mark = f "" where
    f buffer =
        if reverse mark `isPrefixOf` buffer && (length mark == length buffer || buffer!!length mark == '\n')
            then return $ reverse $ drop (length mark) buffer
            else f . (: buffer) =<< hGetChar h


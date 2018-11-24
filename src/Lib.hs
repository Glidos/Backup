module Lib
    ( withSFTP,
    listDirectory
    ) where

import System.IO
import System.Process.Typed
import Data.List
import Data.List.Extra

newtype SFTP = SFTP (Process Handle Handle ())

withSFTP :: String -> (SFTP -> IO a) -> IO a
withSFTP url commands =
    let config = setStdin createPipe
               $ setStdout createPipe
               $ setStderr closed
               $ shell $ "unbuffer -p sftp " ++ url

    in withProcess config $ \p -> do
        hGetUptoMark (getStdout p) "\nsftp>"
        result <- commands (SFTP p)
        hClose (getStdin p)
        checkExitCode p
        return result

listDirectory :: SFTP -> String -> IO [String]
listDirectory (SFTP p) path = do
    hPutStrLn (getStdin p) $ "cd " ++ path -- send change dir command
    hFlush (getStdin p)
    hGetUptoMark (getStdout p) "\nsftp>" -- read the next prompt
    hPutStrLn (getStdin p) "ls -1" -- send command
    hFlush (getStdin p)
    hGetLine (getStdout p) -- Read echoed-back command
    output <-hGetUptoMark (getStdout p) "\nsftp>" -- Get everything upto the next prompt
    return $ map trim $ lines output


-- Read characters from a handle until a marking string is found
-- and return what has been read upto that mark.
--
-- Implment by buffering the read-in characters in reverse order.
hGetUptoMark :: Handle -> String -> IO String
hGetUptoMark h mark = f "" where
    f buffer =
        if reverse mark `isPrefixOf` buffer
            then return $ reverse $ drop (length mark) buffer
            else do
                c <- hGetChar h
                f $ c:buffer

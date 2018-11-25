module SFTP
    ( withSFTP,
    listDirectory,
    upload,
    download,
    deleteFile
    ) where

import System.IO
import System.Process.Typed
import Data.List

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

listDirectory :: SFTP -> String -> IO (Maybe [String])
listDirectory (SFTP p) path = do
    hPutStrLn (getStdin p) $ "cd " ++ path -- send change dir command
    hFlush (getStdin p)
    cdout <- hGetUptoMark (getStdout p) "\nsftp>" -- read the next prompt
    if length (lines cdout) == 1 -- More than one line implies an error message
        then do
            hPutStrLn (getStdin p) "ls -1" -- send list dir command
            hFlush (getStdin p)
            hGetLine (getStdout p) -- Read echoed-back command
            lsout <- hGetUptoMark (getStdout p) "\nsftp>" -- Get everything upto the next prompt
            return $ Just $ map init $ lines lsout -- output is a line per item with an extra CR char on the end of each
        else return Nothing

upload :: SFTP -> String -> String -> IO Bool
upload (SFTP p) lpath rpath = do
    hPutStrLn (getStdin p) $ "put " ++ lpath ++ " " ++ rpath
    hFlush (getStdin p)
    out <- hGetUptoMark (getStdout p) "\nsftp>"
    return $ "100%" `isInfixOf` out -- command reports progress, getting to 100% on success


download :: SFTP -> String -> String -> IO Bool
download (SFTP p) rpath lpath = do
    hPutStrLn (getStdin p) $ "get " ++ rpath ++ " " ++ lpath
    hFlush (getStdin p)
    out <- hGetUptoMark (getStdout p) "\nsftp>"
    return $ "100%" `isInfixOf` out -- command reports progress, geting to 100% on success

deleteFile :: SFTP -> String -> IO Bool
deleteFile (SFTP p) path = do
    hPutStrLn (getStdin p) $ "rm " ++ path
    hFlush (getStdin p)
    out <- hGetUptoMark (getStdout p) "\nsftp>"
    return $ length (lines out) <= 2 -- More than 2 lines implies an error message

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

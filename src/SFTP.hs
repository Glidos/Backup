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
import Data.Bool

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

cd :: SFTP -> String -> IO Bool
cd sftp path = runCommand sftp ("cd " ++ path) (null . lines)

list :: SFTP -> IO (Maybe [String])
list sftp = runCommand sftp "ls -1" (Just . map init . lines)
-- output is a line per item, each with an extra CR char at the end

listDirectory :: SFTP -> String -> IO (Maybe [String])
listDirectory sftp path = cd sftp path >>= bool (return Nothing) (list sftp)


upload :: SFTP -> String -> String -> IO Bool
upload sftp lpath rpath = runCommand sftp ("put " ++ lpath ++ " " ++ rpath) (isInfixOf "100%")
-- command reports progress, getting to 100% on success


download :: SFTP -> String -> String -> IO Bool
download sftp rpath lpath = runCommand sftp ("get " ++ rpath ++ " " ++ lpath) (isInfixOf "100%")
-- command reports progress, geting to 100% on success

deleteFile :: SFTP -> String -> IO Bool
deleteFile sftp path = runCommand sftp ("rm " ++ path) ((<=1) . length. lines)
-- More than 1 line implies an error message

runCommand :: SFTP -> String -> (String -> a) -> IO a
runCommand (SFTP p) cmd interpret = do
    hPutStrLn (getStdin p) cmd
    hFlush (getStdin p)
    hGetLine (getStdout p)
    interpret <$> hGetUptoMark (getStdout p) "sftp>"

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


module SFTP
    ( SFTP,
    withSFTP,
    listDirectory,
    upload,
    download,
    deleteFile
    ) where

import System.IO                        (Handle, hPutStrLn, hGetChar, hGetLine, hFlush, hClose, hGetContents)
import System.Process.Typed             (Process, getStdin, getStdout, setStdin, setStdout, setStderr, closed, createPipe, shell, withProcess, checkExitCode)
import Data.List                        (isInfixOf, isPrefixOf)
import Data.List.Split                  (splitWhen)
import Control.Conditional              (select)
import Control.Monad.Trans.State.Strict (StateT, evalStateT, runStateT, get, state)
import Control.Monad.IO.Class           (liftIO)

import Util (dropFromEnd)

-- Use the State monad to carry the handle to which to send commands and the
-- as yet unconsumed output. The output is split into lines and then those
-- lines grouped into replies by treating lines that begin "sftp>" as
-- delimiters.
type SFTP a = StateT (Handle, [[String]]) IO a

withSFTP :: String -> SFTP a -> IO a
withSFTP url commands =
    let config = setStdin createPipe
               $ setStdout createPipe
               $ setStderr closed
               $ shell $ "unbuffer -p sftp " ++ url

    in withProcess config $ \p -> do
        replies <- parseOutput <$> hGetContents (getStdout p)
        result <- evalStateT (consumeReply >> commands) (getStdin p, replies)
        hClose (getStdin p)
        hClose (getStdout p)
        checkExitCode p
        return result

cd :: String -> SFTP ()
cd path = runCommand (unwords ["cd", path]) null (const ())

list :: SFTP [String]
list = runCommand "ls -1" (const True) (map init)
-- output is a line per item, each with an extra CR char at the end

listDirectory :: String -> SFTP [String]
listDirectory path = cd path >> list


upload :: String -> String -> SFTP ()
upload lpath rpath = runCommand (unwords ["put", lpath, rpath]) (any (isInfixOf "100%")) (const ())
-- command reports progress, getting to 100% on success


download :: String -> String -> SFTP ()
download rpath lpath = runCommand (unwords ["get", rpath, lpath]) (any (isInfixOf "100%")) (const ())
-- command reports progress, geting to 100% on success

deleteFile :: String -> SFTP ()
deleteFile path = runCommand (unwords ["rm", path]) ((<=1) . length) (const ())
-- More than 1 line implies an error message


parseOutput :: String -> [[String]]
parseOutput = splitWhen (isPrefixOf "sftp>") . lines

getCmdHandle :: SFTP Handle
getCmdHandle = fst <$> get

consumeReply :: SFTP [String]
consumeReply = state $ \(h, r:rs) -> (r, (h, rs))


runCommand :: String -> ([String] -> Bool) -> ([String] -> a) -> SFTP a
runCommand cmd test value =
    getCmdHandle >>= \h -> liftIO (hPutStrLn h cmd)
                            >> liftIO (hFlush h)
                            >> consumeReply
                            >>= select test (return . value) (fail . dropFromEnd 2 . last)


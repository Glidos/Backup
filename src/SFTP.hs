module SFTP
    ( SFTP,
    withSFTP,
    withSFTPE,
    listDirectory,
    upload,
    download,
    deleteFile
    ) where

import System.Exit                      (ExitCode(ExitSuccess, ExitFailure))
import System.IO                        (Handle, hPutStrLn, hFlush, hClose, hGetContents)
import System.Process.Typed             (Process, getStdin, getStdout, setStdin, setStdout, setStderr, closed, createPipe, shell, withProcess, waitExitCode)
import Data.List                        (isInfixOf, isPrefixOf)
import Data.List.Split                  (splitWhen)
import Control.Conditional              (select)
import Control.Monad.Trans.Class        (lift)
import Control.Monad.Trans.Except       (ExceptT, runExceptT, throwE)
import Control.Monad.Trans.State.Strict (StateT, evalStateT, get, state)
import Control.Monad.IO.Class           (liftIO)

import Util (dropFromEnd)

-- Use the State monad to carry the handle to which to send commands and the
-- as yet unconsumed output. The output is split into lines and then those
-- lines grouped into replies by treating lines that begin "sftp>" as
-- delimiters.
type SFTP = ExceptT String (StateT (Handle, [[String]]) IO)

withSFTP :: String -> SFTP a -> IO a
withSFTP url commands =  withSFTPE url commands >>= either fail return

withSFTPE :: String -> SFTP a -> IO (Either String a)
withSFTPE url commands =
    let config = setStdin createPipe
               $ setStdout createPipe
               $ setStderr closed
               $ shell $ "unbuffer -p sftp " ++ url

    in withProcess config $ \p -> do
        replies <- parseOutput <$> hGetContents (getStdout p)
        result <- (evalStateT $ runExceptT $ consumeReply >> commands) (getStdin p, replies)
        hClose (getStdin p)
        hClose (getStdout p)
        code <- waitExitCode p
        return $ case code of ExitSuccess -> result
                              ExitFailure i -> Left $ "sftp returned: " ++ show i

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
getCmdHandle = fst <$> lift get

consumeReply :: SFTP [String]
consumeReply = lift $ state $ \(h, r:rs) -> (r, (h, rs))


runCommand :: String -> ([String] -> Bool) -> ([String] -> a) -> SFTP a
runCommand cmd test value =
    getCmdHandle >>= \h -> liftIO (hPutStrLn h cmd)
                            >> liftIO (hFlush h)
                            >> consumeReply
                            >>= select test (return . value) (throwE . dropFromEnd 2 . last)


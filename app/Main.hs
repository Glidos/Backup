module Main where

import Host
import SFTP
import           Control.Exception

test :: Show a => IO a -> IO ()
test command = (command >>= print) `catch` \e -> print $ "Error: " ++ show (e :: IOException)

main :: IO ()
main = withSFTP (remoteUser ++ "@" ++ remoteHost) $ \sftp -> do
    test $ listDirectory sftp remoteDir
    test $ listDirectory sftp (remoteDir ++ "/y")
    test $ upload sftp "/home/paul/crontab" (remoteDir ++ "/crontab")
    test $ upload sftp "/home/paul/crontabx" (remoteDir ++ "/crontab")
    test $ upload sftp "/home/paul/crontab" (remoteDir ++ "/y/crontab")
    test $ listDirectory sftp remoteDir
    test $ download sftp (remoteDir ++ "/crontab") "/home/paul/crontabz"
    test $ download sftp (remoteDir ++ "/crontabx") "/home/paul/crontabz"
    test $ download sftp (remoteDir ++ "/crontab") "/home/paulx/crontabz"
    test $ deleteFile sftp (remoteDir ++ "/crontab")
    test $ listDirectory sftp remoteDir
    test $ deleteFile sftp (remoteDir ++ "/crontabx")

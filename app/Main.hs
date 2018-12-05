module Main where

import Host
import SFTP

main :: IO ()
main = withSFTP (remoteUser ++ "@" ++ remoteHost) $ \sftp -> do
    listDirectory sftp remoteDir >>= print
    listDirectory sftp (remoteDir ++ "/y") >>= print
    upload sftp "/home/paul/crontab" (remoteDir ++ "/crontab") >>= print
    upload sftp "/home/paul/crontabx" (remoteDir ++ "/crontab") >>= print
    upload sftp "/home/paul/crontab" (remoteDir ++ "/y/crontab") >>= print
    listDirectory sftp remoteDir >>= print
    download sftp (remoteDir ++ "/crontab") "/home/paul/crontabz" >>= print
    download sftp (remoteDir ++ "/crontabx") "/home/paul/crontabz" >>= print
    download sftp (remoteDir ++ "/crontab") "/home/paulx/crontabz" >>= print
    deleteFile sftp (remoteDir ++ "/crontab") >>= print
    listDirectory sftp remoteDir >>= print
    deleteFile sftp (remoteDir ++ "/crontabx") >>= print

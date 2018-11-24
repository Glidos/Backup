module Main where

import Host
import SFTP

main :: IO ()
main = withSFTP (remoteUser ++ "@" ++ remoteHost) (`listDirectory` remoteDir) >>= print

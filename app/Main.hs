module Main where

import Host
import Lib

main :: IO ()
main = withSFTP (remoteUser ++ "@" ++ remoteHost) (`listDirectory` remoteDir) >>= print

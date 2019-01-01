module BackupDir
( BackupDir
, baseDir
, display
, subDir
, wrapperDir
, path
, outerPath
, remove
, removeArchive
, compress
, makeHash
, upload
, remoteItems
, remoteOkay
) where

import Data.List
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Lazy.Char8 as LC
import Data.Maybe
import System.FilePath.Posix
import System.Directory
import System.Process.Typed
import Host
import qualified SFTP
import Text.Regex
import Network.HTTP.Simple

baseDir = "/home/backup"

-- Attributes to do with keeping backup information on disc in directories

class BackupDir a where
    display :: a -> String
    subDir :: a -> Maybe String -- Some backups are grouped in subdirectories (e.g. Diffs)
    wrapperDir :: a -> Maybe String -- Some backups are stored in wrapper directories (e.g. monthly copies in YYYYMM)
    path :: a -> String -- Where the backup is kept
    outerPath :: a -> String -- What to delete to remove the backup (the wrapper if there is one, otherwise same as path)
    path bk = foldr1 (</>) $ [baseDir] ++ maybeToList (subDir bk) ++ maybeToList (wrapperDir bk) ++ [display bk]
    outerPath bk = foldr1 (</>) $ [baseDir] ++ maybeToList (subDir bk) ++ [fromMaybe (display bk) (wrapperDir bk)]

    archiveName :: a -> String
    archiveName bk = display bk ++ ".tar.gpg"

    archivePath :: a -> String
    archivePath bk = baseDir </> "Archives" </> archiveName bk

    checksumPath :: a -> String
    checksumPath bk = baseDir </> "checksums" </> archiveName bk

    remove :: a -> IO ()
    remove bk = removeDirectoryRecursive $ outerPath bk

    removeArchive :: a -> IO ()
    removeArchive bk = removeFile $ archivePath bk

    remoteHash :: a -> IO C.ByteString
    remoteHash bk = head . C.words . getResponseBody <$> (httpBS =<< parseRequest ("http://" ++ httpHost ++ "/md5sum.php?file=" ++ httpDir </> archiveName bk))

    storedHash :: a -> IO C.ByteString
    storedHash bk = C.readFile $ checksumPath bk

    remoteOkay :: a -> IO Bool
    remoteOkay bk = (==) <$> remoteHash bk <*> storedHash bk

    compress :: a -> IO String
    compress bk = createDirectoryIfMissing False (takeDirectory $ archivePath bk)
                  >> runProcess_ (shell $ "cd " ++ takeDirectory (path bk) ++ "; tar -c " ++ display bk ++ " | gpg --batch -c --compress-algo bzip2 --passphrase-file ~/passphrase - > " ++ archivePath bk)
                  >> return (archivePath bk)

    makeHash :: a -> IO ()
    makeHash bk = createDirectoryIfMissing False (takeDirectory $ checksumPath bk)
                  >> (writeFile (checksumPath bk) . head . words . LC.unpack . fst =<< readProcess_ (shell $ "md5sum " ++ archivePath bk))

    upload :: a -> IO ()
    upload bk = SFTP.withSFTP (remoteUser ++ "@" ++ remoteHost) $ \sftp -> SFTP.upload sftp (archivePath bk) (remoteDir </> archiveName bk)

remoteItems :: IO [String]
remoteItems = SFTP.withSFTP (remoteUser ++ "@" ++ remoteHost) $ \sftp ->
    mapMaybe (fmap head . matchRegex (mkRegex "^(.*)\\.tar\\.gpg$")) <$> SFTP.listDirectory sftp remoteDir


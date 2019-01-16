module BackupDir
( BackupDir
, baseDir
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
, removeRemotes
) where

import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Lazy.Char8 as LC
import           Control.Exception     (tryJust)
import           Control.Monad         (guard)
import           Data.Maybe            (maybeToList, fromMaybe, mapMaybe)
import           Data.Foldable         (traverse_)
import           System.FilePath.Posix ((</>), takeDirectory)
import           System.Directory      (removeDirectoryRecursive, removeFile, createDirectoryIfMissing)
import           System.Process.Typed  (runProcess_, readProcess_, shell)
import           System.IO.Error       (isDoesNotExistError)
import           Text.Regex            (mkRegex, matchRegex)
import           Network.HTTP.Simple   (parseRequest, httpBS, getResponseBody)

import           Host                  (httpHost, httpDir, remoteUser, remoteHost, remoteDir)
import qualified SFTP

baseDir = "/home/backup"

-- Attributes to do with keeping backup information on disc in directories

class Show a => BackupDir a where
    subDir :: a -> Maybe String -- Some backups are grouped in subdirectories (e.g. Diffs)
    wrapperDir :: a -> Maybe String -- Some backups are stored in wrapper directories (e.g. monthly copies in YYYYMM)
    path :: a -> String -- Where the backup is kept
    outerPath :: a -> String -- What to delete to remove the backup (the wrapper if there is one, otherwise same as path)
    path bk = foldr1 (</>) $ [baseDir] ++ maybeToList (subDir bk) ++ maybeToList (wrapperDir bk) ++ [show bk]
    outerPath bk = foldr1 (</>) $ [baseDir] ++ maybeToList (subDir bk) ++ [fromMaybe (show bk) (wrapperDir bk)]

archiveName :: BackupDir a => a -> String
archiveName bk = show bk ++ ".tar.gpg"

archivePath :: BackupDir a => a -> String
archivePath bk = baseDir </> "Archives" </> archiveName bk

checksumPath :: BackupDir a => a -> String
checksumPath bk = baseDir </> "checksums" </> archiveName bk

remove :: BackupDir a => a -> IO ()
remove bk = removeDirectoryRecursive $ outerPath bk

removeArchive :: BackupDir a => a -> IO ()
removeArchive bk = removeFile $ archivePath bk

remoteHash :: BackupDir a => a -> IO C.ByteString
remoteHash bk = head . C.words . getResponseBody <$> (httpBS =<< parseRequest ("http://" ++ httpHost ++ "/md5sum.php?file=" ++ httpDir </> archiveName bk))

storedHash :: BackupDir a => a -> IO C.ByteString
storedHash bk = C.readFile $ checksumPath bk

remoteOkay :: BackupDir a => a -> IO Bool
remoteOkay bk = either (const False) id <$> tryJust (guard . isDoesNotExistError) ((==) <$> remoteHash bk <*> storedHash bk)

compress :: BackupDir a => a -> IO String
compress bk = createDirectoryIfMissing False (takeDirectory $ archivePath bk)
                >> runProcess_ (shell $ "cd " ++ takeDirectory (path bk) ++ "; tar -c " ++ show bk ++ " | gpg --batch -c --compress-algo bzip2 --passphrase-file ~/passphrase - > " ++ archivePath bk)
                >> return (archivePath bk)

makeHash :: BackupDir a => a -> IO ()
makeHash bk = createDirectoryIfMissing False (takeDirectory $ checksumPath bk)
                >> (writeFile (checksumPath bk) . head . words . LC.unpack . fst =<< readProcess_ (shell $ "md5sum " ++ archivePath bk))

upload :: BackupDir a => a -> IO ()
upload bk = SFTP.withSFTP (remoteUser ++ "@" ++ remoteHost) $ \sftp -> SFTP.upload sftp (archivePath bk) (remoteDir </> archiveName bk)

removeRemotes :: BackupDir a => [a] -> IO ()
removeRemotes remotes = SFTP.withSFTP (remoteUser ++ "@" ++ remoteHost) $ \sftp ->
    traverse_ (SFTP.deleteFile sftp . (</>) remoteDir . archiveName) remotes
    >> traverse_ (tryJust (guard . isDoesNotExistError) . removeFile . checksumPath) remotes

remoteItems :: IO [String]
remoteItems = SFTP.withSFTP (remoteUser ++ "@" ++ remoteHost) $ \sftp ->
    mapMaybe (fmap head . matchRegex (mkRegex "^(.*)\\.tar\\.gpg$")) <$> SFTP.listDirectory sftp remoteDir


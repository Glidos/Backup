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
, decompress
, showDiff
, addFiles
, generateFileList
, applyFileList
, makeHash
, upload
, downloadL
, remoteArchives
, localArchives
, remoteOkay
, removeRemotes
) where

import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Lazy.Char8 as LC
import           Control.Exception     (tryJust, handleJust)
import           Control.Monad         (guard)
import           Control.Monad.Trans.Select
import           Control.Monad.Extra
import           Data.Maybe            (maybeToList, fromMaybe, mapMaybe)
import           Data.Foldable         (traverse_)
import           Data.Bool             (bool)
import           Data.List             ((\\))
import           System.FilePath.Posix ((</>), takeDirectory, takeFileName)
import           System.Directory      (removeDirectoryRecursive, removeFile, removeDirectory,
                                        createDirectoryIfMissing, listDirectory, doesPathExist, doesDirectoryExist)
import           System.Process.Typed  (runProcess_, readProcessStdout_, shell)
import           System.IO.Error       (isDoesNotExistError)
import           Text.Regex            (mkRegex, matchRegex)
import           Text.Printf           (printf)
import           Network.HTTP.Simple   (parseRequest, httpBS, getResponseBody)

import           Host                  (httpHost, httpDir, remoteUser, remoteHost, remoteDir)
import qualified SFTP
import           Util                  (setMinus)

baseDir = "/home/backup"

md5sumUrl :: String -> String -> String -> String
md5sumUrl = printf "http://%s/md5sum.php?file=%s/%s"

compressCmd :: String -> String -> String
compressCmd backupPath archivePath
    = printf "cd %s && tar -c %s | gpg --batch -c --compress-algo bzip2 --passphrase-file ~/passphrase - > %s"
             (takeDirectory backupPath) (takeFileName backupPath) archivePath

decompressCmd :: String -> String -> String
decompressCmd backupPath archivePath
    = printf "cd %s && gpg --batch -d --compress-algo bzip2 --passphrase-file ~/passphrase %s | tar -x"
             (takeDirectory backupPath) archivePath

diffCmd :: String -> String -> String
diffCmd = printf "diff -q -r %s %s"

addFilesCmd :: String -> String -> String
addFilesCmd = printf "shopt -s dotglob && cp -fal %s/* %s"

listFilesCmd :: String -> String
listFilesCmd = printf "cd %s && find ."

md5sumCmd :: String -> String
md5sumCmd = printf "md5sum %s"

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
remoteHash bk = head . C.words . getResponseBody <$> (httpBS =<< parseRequest (md5sumUrl httpHost httpDir (archiveName bk)))

storedHash :: BackupDir a => a -> IO C.ByteString
storedHash bk = C.readFile $ checksumPath bk

remoteOkay :: BackupDir a => a -> IO Bool
remoteOkay bk = handleJust (guard . isDoesNotExistError)
                           (\_ -> return False)
                           ((==) <$> remoteHash bk <*> storedHash bk)

compress :: BackupDir a => a -> IO String
compress bk = createDirectoryIfMissing False (takeDirectory $ archivePath bk)
                >> runProcess_ (shell $ compressCmd (path bk) (archivePath bk))
                >> return (archivePath bk)

decompress :: BackupDir a => a -> IO ()
decompress bk = createDirectoryIfMissing False (takeDirectory $ path bk)
                    >> runProcess_ (shell $ decompressCmd (path bk) (archivePath bk))

showDiff :: (BackupDir a, BackupDir b) => a -> b -> IO ()
showDiff bk1 bk2 = LC.putStr =<< readProcessStdout_ (shell $ diffCmd (path bk1) (path bk2))

addFiles :: (BackupDir a, BackupDir b)  => a -> b -> IO ()
addFiles tgt src = createDirectoryIfMissing False (path tgt)
                    >> runProcess_ (shell $ addFilesCmd (path src) (path tgt))

generateFileList :: BackupDir a => a -> IO ()
generateFileList bk =
    writeFile (path bk </> "file_list") . LC.unpack =<< readProcessStdout_ (shell $ listFilesCmd $ path bk)

applyFileList :: BackupDir a => a -> IO ()
applyFileList bk = do
    currentFileList <- lines . LC.unpack <$> readProcessStdout_ (shell $ listFilesCmd $ path bk)
    desiredFileList <- lines <$> readFile (path bk </> "file_list")
    traverse_ ((\fp -> whenM (doesPathExist fp)
                        $ ifM (doesDirectoryExist fp)
                            (removeDirectory fp)
                            (removeFile fp)
               ) . (path bk </>)
              ) $ reverse (currentFileList `setMinus` desiredFileList)

makeHash :: BackupDir a => a -> IO ()
makeHash bk = createDirectoryIfMissing False (takeDirectory $ checksumPath bk)
                >> (writeFile (checksumPath bk) . head . words . LC.unpack
                        =<< readProcessStdout_ (shell $ md5sumCmd $ archivePath bk))

upload :: BackupDir a => a -> IO ()
upload bk = SFTP.withSFTP_ (remoteUser ++ "@" ++ remoteHost)
                    $ SFTP.upload (archivePath bk) (remoteDir </> archiveName bk)

downloadL :: BackupDir a => [a] -> IO ()
downloadL bks = SFTP.withSFTP_ (remoteUser ++ "@" ++ remoteHost)
                    $ traverse_ (\bk -> SFTP.download (remoteDir </> archiveName bk) $ archivePath bk) bks

removeRemotes :: BackupDir a => [a] -> IO ()
removeRemotes remotes = SFTP.withSFTP (remoteUser ++ "@" ++ remoteHost)
    (traverse_ (SFTP.deleteFile . (remoteDir </>) . archiveName) remotes)
    >> traverse_ (tryJust (guard . isDoesNotExistError) . removeFile . checksumPath) remotes

parseArchive :: String -> Maybe String
parseArchive = fmap head . matchRegex (mkRegex "^(.*)\\.tar\\.gpg$")

remoteArchives :: IO [String]
remoteArchives = SFTP.withSFTP_ (remoteUser ++ "@" ++ remoteHost) $
    mapMaybe parseArchive <$> SFTP.listDirectory remoteDir

localArchives :: IO [String]
localArchives = mapMaybe parseArchive <$> ifM (doesDirectoryExist $ baseDir </> "Archives")
                                              (listDirectory $ baseDir </> "Archives")
                                              (return [])

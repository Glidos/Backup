module BackupDir
( BackupDir
, display
, subDir
, outerPath
, path
, baseDir
) where

baseDir = "/home/backup"


class BackupDir a where
    display :: a -> String
    subDir :: a -> Maybe String -- Some backup copies are placed in subdirectories
    path :: a -> String -- Where the backup is kept
    outerPath :: a -> String -- What to delete to remove the backup
    outerPath bk = baseDir ++ "/" ++ case subDir bk of Nothing -> display bk
                                                       Just dir -> dir
    path bk = case subDir bk of Nothing -> outerPath bk
                                Just _ -> outerPath bk ++ "/" ++ display bk


module BackupDir
( BackupDir
, baseDir
, display
, subDir
, wrapperDir
, path
, outerPath
) where

import Data.List
import Data.Maybe
import System.FilePath.Posix

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


# Backup

This is the program with which I backup data on my home server, orginally written in Shell Script, later in Ruby, now in Haskell. I've uploaded it to GitHub more to get feedback on my Haskell usage (my first use of Haskell) than in anticipation of its being used, but anyone is welcome to.

There are two misfortunes that the program attempts to mitigate: it keeps multiple copies of files locally in case of accidental deletion of a needed file, and it uploads files off site in case of disc crash or fire!

### Local Copies

Copies of files are kept in `/home/backup` in subdirectories named according to date `YYYY-MM-DD`. These are created daily using rsync, specifying the location of the previous one to the rysnc option `--link-dest`, so that - after creation of the first backup - further ones use disc space only for new and changed files. They are eventually deleted, keeping only the most recent 7 (usually a weeks worth, assuming no missed backup days).

Further weekly, monthly and yearly copies are made two directories down, respectively `YYYY-WeekNN/YYYY-MM-DD`, `YYYY-MM/YYYY-MM-DD` and `YYYY/YYYY-MM-DD`. These are created using `cp -al`, so again little disc space is used. The most recent 4 of the weekly copies are kept, 12 of the monthly, and the yearly are kept for ever. I'm not at all sure this is an ideal scheme, but the idea is that a working file kept for at least a day will hang around in the back up system for at least a week; a file kept for at least a week, will be backed up for at least a month, etc.

### Remote Copies

At the time this program was originally devised, both external server space and my internet connection bandwidth made it impractical to upload entire backups (still true to some extent), and so an incremental system of uploading diffs is used, with the backup that acts as the seed for the diffs (a single entire backup) requiring a partly-manual creation procedure (burning several copies to DVDs and handing to trusted friends).

Several incremental levels are used. The seed backup is called Level 1. Based on Level 1, a monthly renewable Level 2 is produced, and in turn a weekly and daily renewable Level 3 and 4. The creation of each diff requires the presence of the full backup on which it is based, so entire copies of Levels 1 to 3 have to be kept locally on disc. Like the copies mentioned in the previous section, these are created using `cp -al` and are held in directories named `LevelN/YYYY-MM-DD`.

Each diff is archived, compressed and gpg encrypted before being uploaded. Uploading is performed using SFTP. A rudimentary Haskell SFTP implementation that uses the linux sftp command is included in this source. The last stage is to check the status of previously uploaded files and delete any considered no longer needed. Checking is performed by comparing the md5sum hash with a hash recorded before upload. The hashing of the remote file is performed by a php script on the remote server, assumed accessible via http. A check of the hash is also made in early stages of the backup process, when chosing which level needs recreating, so that new diffs that rely on corrupted files are never generated.

After each upload, a calculation is made of the diffs needed to recreate the last two days backups. Any others are deleted from the remote server.

### Use of Exceptions

There are several eventualities, such as directories being missing or extra ones present, that should never happen in normal usage but need to be checked for. These are handled by raising exceptions. That choice was made because none of the cases can be dealt with by the program itself and manual intervention is needed, so it's best if the program just stops at the point of detecting the unexpected state. Exceptions seem ideal. The program consists of many IO calls being made purely for their side-effect with no value explicitly passed from one step to the next. I believe, to handle the error cases with Maybe values and still have the program stop, would require artificially passing values from one step to the next.

### Deficiencies

There are other parts of the process that have yet to be coded in Haskell and still exist only in the Ruby version. This Haskell version doesn't yet aid in the manual creation and checking of the Level 1 basis for the remote uploads. Nor does it support downloading and combing the remote diffs in preparation for the restore process.

The SFTP implementation assumes authentication is handled via ssh keys. Currently, if the sftp command requests a password, the program will hang until time out.

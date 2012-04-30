module Notifier where

import Control.Monad
import System.Directory
import System.FilePath
import System.INotify

setupNotifications :: FilePath -> (String -> IO ()) -> IO ()
setupNotifications dir notifyFileChanged = do
    i <- initINotify
    allDirs <- getDirsRecursive dir
    forM_ allDirs $ \dir ->
        addWatch i [Modify] dir (handle dir)
    where
        handle rootDir (Modified _ (Just f)) = notifyFileChanged (makeRelative dir (rootDir </> f))
        handle rootDir _                     = return ()

getDirsRecursive :: FilePath -> IO [FilePath]
getDirsRecursive rootDir = do
    contents  <- getDirectoryContents rootDir
    innerDirs <- forM (filter (`notElem` [".", ".."]) contents) $ \path -> do
        let fullPath = rootDir </> path
        isDirectory <- doesDirectoryExist fullPath
        if isDirectory
            then getDirsRecursive fullPath
            else return []
    return $ rootDir:concat innerDirs

module Notifier where

import System.INotify

setupNotifications :: FilePath -> (String -> IO ()) -> IO ()
setupNotifications dir nofifyFileChanged = do
    i <- initINotify
    addWatch i [Modify, MoveIn, MoveOut] dir $ \e -> do
        putStrLn (show e)
        nofifyFileChanged ""
    return ()

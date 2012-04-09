module Notifier where

import System.INotify

setupNotifications :: FilePath -> (String -> IO ()) -> IO ()
setupNotifications dir notifyFileChanged = do
    i <- initINotify
    addWatch i [Modify, MoveIn, MoveOut] dir handle
    return ()
    where
        handle (Modified _ (Just f)) = notifyFileChanged f
        handle _                     = return ()

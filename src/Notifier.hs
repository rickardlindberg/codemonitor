module Notifier where

import System.INotify

setupNotifications :: FilePath -> (String -> IO ()) -> IO ()
setupNotifications dir nofifyFileChanged = do
    i <- initINotify
    addWatch i [Modify, MoveIn, MoveOut] dir handle
    return ()
    where
        handle (Modified _ (Just f)) = nofifyFileChanged f
        handle _                     = return ()

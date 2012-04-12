-- Expect to get no message when killing thread

import Control.Concurrent

main = do
    mvar <- newEmptyMVar
    id <- forkIO $ waitForMessage "first" mvar

    threadDelay $ 1*1000000

    killThread id
    putMVar mvar "secret code"

    id <- forkIO $ waitForMessage "second" mvar

    threadDelay $ 1*1000000
    putStrLn "done"

waitForMessage name mvar = do
    putStrLn $ "(" ++ name ++ ") waiting for message"
    message <- readMVar mvar
    putStrLn $ "got message: " ++ message

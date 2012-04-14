import Control.Concurrent

main = do
    id <- forkIO $ runAThread

    threadDelay $ 2*1000000

    putStrLn $ "time to kill" ++ (show id)
    killThread id

    putStrLn "done"

runAThread = do
    putStrLn $ "running"
    threadDelay $ 1*1000000
    putStrLn $ "done"

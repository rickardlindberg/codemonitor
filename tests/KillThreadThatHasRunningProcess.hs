import Control.Concurrent
import Control.Exception
import Control.Monad
import qualified Control.Exception as C
import System.Exit
import System.IO
import System.Process

main = do
    id <- forkIO longCommand
    putStrLn "thread running"
    threadDelay $ 10*1000000
    putStrLn "thread killed"
    killThread id
    threadDelay $ 10*1000000
    putStrLn "app died killed"

longCommand = do
    (exit, stdout, stderr) <- myReadProcessWithExitCode "sh" ["-c", "while true; do echo 1; sleep 1; done"] ""
    return ()



myReadProcessWithExitCode
    :: FilePath                 -- ^ command to run
    -> [String]                 -- ^ any arguments
    -> String                   -- ^ standard input
    -> IO (ExitCode,String,String) -- ^ exitcode, stdout, stderr
myReadProcessWithExitCode cmd args input = do
    (Just inh, Just outh, Just errh, pid) <-
        createProcess (proc cmd args){ std_in  = CreatePipe,
                                       std_out = CreatePipe,
                                       std_err = CreatePipe }

    let doIt = do
        outMVar <- newEmptyMVar

        -- fork off a thread to start consuming stdout
        out  <- hGetContents outh
        _ <- forkIO $ C.evaluate (length out) >> putMVar outMVar ()

        -- fork off a thread to start consuming stderr
        err  <- hGetContents errh
        _ <- forkIO $ C.evaluate (length err) >> putMVar outMVar ()

        -- now write and flush any input
        unless (null input) $ do hPutStr inh input; hFlush inh
        hClose inh -- done with stdin

        -- wait on the output
        takeMVar outMVar
        takeMVar outMVar
        hClose outh
        hClose errh

        -- wait on the process
        ex <- waitForProcess pid

        return (ex, out, err)

    finally doIt (putStrLn "killing pid" >> terminateProcess pid)

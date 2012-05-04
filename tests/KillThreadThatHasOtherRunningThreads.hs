import Control.Concurrent
import Control.Exception
import Control.Monad
import qualified Control.Exception as C
import System.Exit
import System.IO
import System.Process

main = do
    id <- forkIO $ startPrinter "one"
    threadDelay $ 10*1000000
    killThread id
    putStrLn "killing one parent"
    threadDelay $ 10*1000000
    putStrLn "done"

startPrinter text = do
    idPrinter <- forkIO $ printInThread text
    finally (printInThread $ "running: " ++ text) (putStrLn "i got aborted")

printInThread text = forever $ do
    putStrLn text
    threadDelay $ 2*1000000

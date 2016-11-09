module Main where
import Network
import Network.Socket hiding (accept)
import System.Environment
import System.IO
import Control.Concurrent 
import Control.Concurrent.STM
import Control.Exception
import Data.List.Split
import Data.Word

data Server = Server { address :: String, port :: String }

newServer :: String -> String -> Server
newServer addr portnum = Server { address = addr, port = portnum }

maxnumThreads = 4

main:: IO ()
main = withSocketsDo $ do

  --Command line arguments for port and address
  args <- getArgs
  let address = args !! 0
  let port = args !! 1

  --Listen on port from command line argument
  sock <- listenOn $ PortNumber $ fromIntegral $ read port

  --Let the user know which port
  putStrLn $ "Listening on " ++ port
  
  --New Abstract FIFO Channel
  chan <- newChan
  
  --Tvars are variables Stored in memory, this way we can access the numThreads from any method
  numThreads <- atomically $ newTVar 0

  --New server data type defined above
  let server = newServer address port

  --Spawns a new thread to handle the clientconnectHandler method, passes socket, channel, numThreads and server
  forkIO $ clientconnectHandler sock chan numThreads server
  
  --Calls the mainHandler which will monitor the FIFO channel
  mainHandler sock chan

mainHandler :: Socket -> Chan String -> IO ()
mainHandler sock chan = do

  --Read current message on the FIFO channel
  chanMsg <- readChan chan

  --If KILL_SERVICE, stop mainHandler running, If anything else, call mainHandler again, keeping the service running
  case (chanMsg) of
    ("KILL_SERVICE") -> putStrLn "Terminating the Service!"
    _ -> mainHandler sock chan

clientconnectHandler :: Socket -> Chan String -> TVar Int -> Server -> IO ()
clientconnectHandler sock chan numThreads server = do

  --Accept the socket which returns a handle, host and port
  (handle, host, port) <- accept sock
  --handle <- socketToHandle s ReadWriteMode
  --Read numThreads from memory and print it on server console
  count <- atomically $ readTVar numThreads
  putStrLn $ "numThreads = " ++ show count

  --If there are still threads remaining...
  if (count < maxnumThreads) then do
    myForkFinally (clientHandler handle chan server numThreads) (\_ -> atomically $ decrementTVar numThreads)
    atomically $ incrementTVar numThreads
    else do
      hPutStrLn handle "Service reached maximum capacity, please try again later!"
      hClose handle

  clientconnectHandler sock chan numThreads server

clientHandler :: Handle -> Chan String -> Server -> TVar Int -> IO ()
clientHandler handle chan server numThreads = do
  line <- hGetLine handle
  let cmd = words line

  case (head cmd) of
    ("HELO") -> heloCommand handle chan server numThreads (unwords (tail cmd))
    ("KILL_SERVICE") -> killCommand handle chan
    _ -> do hPutStrLn handle ("Unknown Command - " ++ line)
  
  clientHandler handle chan server numThreads

heloCommand :: Handle -> Chan String -> Server -> TVar Int -> String -> IO ()
heloCommand handle chan server@Server{..} numThreads msg = do
  writeChan chan "HELO command processed!"

  hPutStrLn handle $ "HELO " ++ msg ++ "\n\
                     \IP:" ++ address ++ "\n\
                     \Port:" ++ port ++ "\n\
                     \StudentID:12306421"

  hFlush handle

killCommand :: Handle -> Chan String -> IO ()
killCommand handle chan = do
  hPutStrLn handle "Service is now terminating!"
  writeChan chan "KILL_SERVICE"

incrementTVar :: TVar Int -> STM ()
incrementTVar tv = modifyTVar tv ((+) 1)

decrementTVar :: TVar Int -> STM ()
decrementTVar tv = modifyTVar tv (subtract 1)

myForkFinally :: IO a -> (Either SomeException a -> IO ()) -> IO ThreadId
myForkFinally action and_then =
  mask $ \restore ->
    forkIO $ try (restore action) >>= and_then

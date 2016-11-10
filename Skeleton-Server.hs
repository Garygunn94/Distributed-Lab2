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

--Server data type allows me to pass address and port details easily
data Server = Server { address :: String, port :: String }

--Constructor
newServer :: String -> String -> Server
newServer addr portnum = Server { address = addr, port = portnum }

--4 is easy for testing the pooling
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

  --If there are still threads remaining create new thread and increment (exception if thread is lost -> decrement), else tell user capacity has been reached
  if (count < maxnumThreads) then do
    forkFinally (clientHandler handle chan server numThreads) (\_ -> atomically $ decrementTVar numThreads)
    atomically $ incrementTVar numThreads
    else do
      hPutStrLn handle "Maximum number of threads in use. try again soon"
      hClose handle

  clientconnectHandler sock chan numThreads server

clientHandler :: Handle -> Chan String -> Server -> TVar Int -> IO ()
clientHandler handle chan server numThreads = do

  --Recieve message from client
  line <- hGetLine handle
  
  --Seperate into words (seperated by whitespace)
  let cmd = words line

  --Case switch HELO text and KILL_SERVICE are handled, everything else is ignored
  case (head cmd) of
    ("HELO") -> heloCommand handle chan server numThreads (unwords (tail cmd))
    ("KILL_SERVICE") -> killservice handle chan
    _ -> do hPutStrLn handle ("Command not recognised - " ++ line)
  
  clientHandler handle chan server numThreads

--Function called when HELO text command recieved  
heloCommand :: Handle -> Chan String -> Server -> TVar Int -> String -> IO ()
heloCommand handle chan server@Server{..} numThreads msg = do
  writeChan chan "HELO command recieved"

  --Sends the following to the client, as requested
  hPutStrLn handle $ "HELO " ++ msg ++ "\n\
                     \IP:" ++ address ++ "\n\
                     \Port:" ++ port ++ "\n\
                     \StudentID:12306421"

  hFlush handle

--Function called when KILL_SERVICE is recieved - Terminated the service
killservice :: Handle -> Chan String -> IO ()
killservice handle chan = do
  --Let Clients know
  hPutStrLn handle "Terminating the Service!"
  --Send message up to mainHandler
  writeChan chan "KILL_SERVICE"

--Increment Tvar stored in memory i.e. numThreads
incrementTVar :: TVar Int -> STM ()
incrementTVar tv = modifyTVar tv ((+) 1)

--Decrement Tvar stored in memory i.e. numThreads
decrementTVar :: TVar Int -> STM ()
decrementTVar tv = modifyTVar tv (subtract 1)

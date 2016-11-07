import Network (listenOn, withSocketsDo, accept, PortID(..), Socket)
import System.Environment (getArgs)
import System.IO (hSetBuffering, hGetLine, hPutStrLn, BufferMode(..), Handle, hFlush, hClose)
import Control.Concurrent (forkIO)

main :: IO ()
main = withSocketsDo $ do
    args <- getArgs
    let port = fromIntegral (read $ head args :: Int)
    sock <- listenOn $ PortNumber port
    putStrLn $ "Listening on " ++ (head args)
    sockHandler sock

sockHandler :: Socket -> IO ()
sockHandler sock = do
    (handle, _, _) <- accept sock
    hSetBuffering handle NoBuffering
    forkIO $ commandProcessor handle
    sockHandler sock

commandProcessor :: Handle -> IO ()
commandProcessor handle = do
   hPutStrLn handle "Hi, welcome to this service, please input your message"
   name <- hGetLine handle
   --hPutStrLn handle name
   case name of
        "hi" -> do {hPutStrLn handle "HTTP/1.0 200 OK\r\nContent-Length: 5\r\n\r\nPong!\r\n"}
        "KILL_SERVICE\n" -> hPutStrLn handle "KILL"
        _ -> do hPutStrLn handle "Unknown command"
   commandProcessor handle
   
echoCommand :: Handle -> [String] -> IO ()
echoCommand handle cmd = do
    hPutStrLn handle "HTTP/1.0 200 OK\r\nContent-Length: 5\r\n\r\nPong!\r\n"

addCommand :: Handle -> [String] -> IO ()
addCommand handle cmd = do
    hPutStrLn handle "KILL"

msg = "HTTP/1.0 200 OK\r\nContent-Length: 5\r\n\r\nPong!\r\n"

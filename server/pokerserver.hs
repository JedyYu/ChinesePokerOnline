import Network.Socket
import System.IO
import Data.List
import Control.Monad
import Data.Bits
import Network.BSD
import Control.Concurrent
import Control.Exception
import Control.Concurrent.Chan
import Control.Concurrent.MVar
import Control.Monad.Fix (fix)

type HandlerFunc = SockAddr -> String -> IO ()
type Msg = (Int, String)

main::IO()
main = severup plainHandler 0
severup :: HandlerFunc ->Int->IO()
severup handlerfunc counter = withSocketsDo $ do
    chan<-newChan
    --创建一个全局的channel，用于sever的广播lk 
    sock<-socket AF_INET Stream 0
    --创建服务器端socket
    setSocketOption sock KeepAlive 1
    --初始化服务器端socket设置
    bindSocket sock (SockAddrInet 4242 iNADDR_ANY)
    --将服务器的socket地址和端口绑在主机的默认IP的4242端口
    listen sock 3
    --监听端口
    lock <- newMVar()
    forkIO $ msgcounter chan counter
    --开启一个线程实时更新channel
    procrequests lock sock chan 0
    --处理连接请求
    where
        procrequests::MVar()->Socket->Chan Msg->Int->IO()
        procrequests lock master chan nr =
            do
                (connectsock,clientaddr) <- accept master
                --接收socket信息
                handles lock clientaddr ("client connect! the number of player is"++show nr)
                forkIO $ procMessages lock connectsock clientaddr chan nr
                --开启消息处理线程
                return ()
                procrequests lock master chan $! nr+1 
        --由于有几台相连，写死循环
        msgcounter::Chan Msg->Int->IO()
        msgcounter  chan counter= do
            (who, msg) <- readChan chan
            putStrLn ((show counter)++" input: "++msg++"frome forkIO")
            msgcounter chan $!counter+1
        procMessages::MVar()->Socket->SockAddr->Chan Msg->Int->IO()
        procMessages lock connectsock clientaddr chan nr=
            do
                let broadcast msg = writeChan chan (nr,msg)
                --将socket转为handle
                hdl<-socketToHandle connectsock ReadWriteMode
                hSetBuffering hdl NoBuffering         
                --设置handle的设置为NOBufering
                --putStrLn "a"
                --broadcast msg
                chan' <- dupChan chan
                --复制channel
                reader <- forkIO $ fix $ \loop -> do
                    (nr', line) <- readChan chan'
                    --读取Chanel中的信息
                    hPutStrLn hdl "line"
                    --将信息广播
                    putStrLn line
                    loop
                --开启广播线程
                handle (\(SomeException _) -> return ()) $ fix $ \loop ->
                    do
                        line <- liftM init (hGetLine hdl)
                        --接收客户端发送的信息
                        broadcast line
                        --将读取到的客户端信息写入channel
                        loop
                msg<-hGetContents hdl 
                mapM_ (handles lock clientaddr) (lines msg)
                --对于每个客户端的信息都接收显示出来
                --mapM_ :: (Foldable t, Monad m) => (a -> m b) -> t a -> m ()
                --Map each element of a structure to a monadic action, evaluate these actions from left to right,
                --and ignore the results. For a version that doesn't ignore the results see mapM.
                --broadcast msg
                killThread reader
                --结束线程释放内存空间，否则 memory out。
                hClose hdl
                handles lock clientaddr "client disconnected" 
        handles :: MVar () -> HandlerFunc
        -- handle :: MVar () -> SockAddr -> String -> IO ()
        handles lock clientaddr msg =
            withMVar lock
                (\a -> handlerfunc clientaddr msg >> return a)
                --withMVar :: MVar a -> (a -> IO b) -> IO b
--本地显示从客户端接收的信息                
plainHandler :: HandlerFunc
plainHandler addr msg = do
    putStrLn $ "From " ++ show addr ++ ": " ++ msg
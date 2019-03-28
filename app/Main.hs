module Main where

import Control.Concurrent (threadDelay)

import SessionImpl
import SessionMonad

data ConnectionManager = ConnectionManager { IORef [Connection] }

data Connection = Connection { clientQueue :: TVar [ResponseImpl], serverQueue :: TVar [RequestImpl] }

newConnection = do
  undefined

serverSendConn (Connection cq _) = do
  undefined

serverRecvConn (Connection _ sq) = do
  undefined

clientSendConn (Connection _ sq) = do
  undefined

clientRecvConn (Connection cq _) = do
  undefined

userSim u1 u2 u3 = do

  authorize "user1"
  authorize "user2"

  sendMessage "Hi from user1" u1
  sendMessage "Hi from user2" u2
  sendMessage "Hi from user3" u3

  disconnect u1
  renew u2
  disconnect u2
  renew u3

  where
    sendRequest :: RequestImpl -> TVar [RequestImpl] -> IO ()
    sendRequest request u =
      atomically (modifyTVar u (`append` request))
      >> print $ "USER: sent request: " ++ (show request)
      >> threadDelay 1000

    login :: String -> TVar [RequestImpl] -> IO ()
    login username = sendRequest (Authorize username)

    sendMessage :: String -> TVar [RequestImpl] -> IO ()
    sendMessage message = sendRequest (PrintString message)

    disconnect :: TVar [RequestImpl] -> IO ()
    disconnect = sendRequest Disconnect

    renew :: TVar [RequestImpl] -> IO ()
    renew = sendRequest Renew


main :: IO ()
main = do
  user1 <- mkUser
  user2 <- mkUser
  user3 <- mkUser

  forkIO $ userSim user1 user2 user3
  runSessionManager $ do
    undefined

  where
    mkUser :: IO (TVar [RequestImpl])
    mkUser = newTVarIO []

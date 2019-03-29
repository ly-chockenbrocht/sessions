module Main where

import Control.Concurrent (threadDelay)

import SessionImpl
import SessionMonad

connectionManager :: TVar (Maybe Connection)
connectionManager = unsafePerformIO $ newTVarIO []

data Connection = Connection { clientQueue :: TChan ResponseImpl, serverQueue :: TChan RequestImpl }

newConnection = do
  cq <- newTChanIO
  sq <- newTChanIO
  let conn = Connection cq sq
  atomically $ modifyTVar connectionManager (const (Just conn))

serverSendConn (Connection cq _) = atomically . writeTChan cq
serverRecvConn (Connection _ sq) = atomically $ readTChan sq
clientSendConn (Connection _ sq) = atomically . writeTChan sq
clientRecvConn (Connection cq _) = atomically $ readTChan cq

userSim = do

  user1conn1 <- newConnection
  login "user1"
  _ <- sendMessage "Hi from user1" user1conn1
  disconnect user1conn1
---------------------------
  user2conn1 <- newConnection
  login "user2" user2conn1
  _ <- sendMessage "Hi from user2" user2conn1
  newKey <- renew user2conn1
  _ <- disconnect user2conn1

  user2conn2 <- newConnection
  _ <- reopen newKey user2conn2
  _ <- sendMessage "test again user2" user2conn2
  _ <- terminate user2conn2
---------------------------
  user1conn2 <- newConnection
  login "user1" user1conn2

  user2conn3 <- newConnection
  login "user2" user2conn3

  sessionsList <- listSessions user1conn2
  session = concat $ filter ((== "user1") . username . sessionUser) sessionsList
  let bootKey = sessionKey session

  kick bootKey user2conn3

  banned <- sendMessage "test for boot" u1conn2
  when (banned == Banned) $ print "user1 is banned as planned"

  where
    recvResponse :: Connection -> IO ResponseImpl
    recvResponse conn = do
      response <- clientRecvConn conn
      print $ "USER: recv response: " ++ (show response)
      threadDelay 1000
      return response

    sendRequest :: RequestImpl -> Connection -> IO ()
    sendRequest request conn =
      atomically $ clientSendConn conn request
      >> print $ "USER: sent request: " ++ (show request)
      >> threadDelay 1000

    doQuery :: RequestImpl -> Connection -> IO (ResponseImpl)
    doQuery q conn = sendRequest q conn >> recvResponse conn

    listSessions :: Connection -> IO (ResponseImpl)
    listSessions = doQuery ListSessions

    login :: String -> Connection -> IO (ResponseImpl)
    login username = doQuery (Authorize username)

    reopen :: SessionKeyImpl -> Connection -> IO (ResponseImpl)
    reopen sessionKey = doQuery (Reopen sessionKey)

    sendMessage :: String -> Connection -> IO (ResponseImpl)
    sendMessage message = doQuery (PrintString message)

    disconnect :: Connection -> IO (ResponseImpl)
    disconnect = doQuery Disconnect

    terminate :: Connection -> IO (ResponseImpl)
    terminate = doQuery Terminate

    renew :: TVar [RequestImpl] -> IO (ResponseImpl)
    renew = sendRequest Renew >> recvResponse


main :: IO ()
main = do

  forkIO $ userSim
  runSessionManager $ forever $ do
    newConn <- listen


  where
    listen = atomically $ do
      maybeConn <- readTVar connectionManager
      case maybeConn of
        Just conn -> do
          writeTVar connectionManger Nothing
          return $ Just conn
        Nothing -> return Nothing

    recvRequest :: Connection -> IO RequestImpl
    recvRequest conn = do
      request <- serverRecvConn conn
      print $ "SERVER: recv request: " ++ (show request)
      threadDelay 1000
      return request

    sendResponse :: ResponseImpl -> Connection -> IO ()
    sendResponse response conn =
      atomically $ serverSendConn conn response
      >> print $ "SERVER: sent response: " ++ (show response)
      >> threadDelay 1000

    authenticate :: Connection -> IO ()
    authenticate conn = do
      request <- recvRequest conn
      case request of
        Authorize un -> authenticatedSession un $ forkIO $ do
          sendResponse (SessionKey sessionKey)
          forever $ handleQuery
        Reopoen sk -> reopenSession sk $ forkIO $ do
          forever $ handleQuery
        _ -> sendResponse (ResponseString "must authenticate first")


    -- handleQuery :: Connection -> IO ()
    handleRequest session = do
      request <- getUserRequest
      case request of
        PrintString s -> printString s >> sendUserMessage
        Disconnect -> disconnect >> print ("CMD: Disconnect, disconnected")
        Terminate -> endSession >> print ("CMD: Terminate, session ended")
        ListSessions -> listSessions
        KickUser un -> kickUser un

    printString s = print $ "CMD: " ++ s

    terminateSession = undefined
    listSessions = undefined
    kickUser un = undefined


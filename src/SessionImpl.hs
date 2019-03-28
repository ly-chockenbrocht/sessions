module SessionImpl where


data RequestImpl = PrintString String | Authorize String | Disconnect | Renew deriving Show
newtype ResponseImpl = ResponseString String | SessionKey |

type PSessionT m a = ReaderT (TVar SessionImpl) m a deriving (Monad)

instance MonadIO m => AuthenticatedUserSession (PSessionT m) where
  type Request m = RequestImpl -- the sort of requests the type of session has
  type Response m = ResponseImpl -- the sort of responses we send to the user

  -- When we want to do something as a user within the user's session
  --withSession :: SessionImpl -> PSessionT m a -> m ()
  -- withSession = undefined

  -- Users should have access to getting and setting their session key... to some degree
  -- renewing should probably have a default implementation based on get and set

  getSession = lift . atomically . readTVar <=< ask


  -- setSession :: SessionImpl -> PSessionT m ()
  setSession session = do
    sessionVar <- ask
    lift . atomically $ writeTVar sessionVar session

  --renewSessionKey :: PSessionT m SessionKeyImpl
  renewSessionKey = setSessionKey =<< genSessionKey

  --getSessionKey :: PSessionT m SessionKeyImpl
  getSessionKey = sessionKey <$> getSession

  --setSessionKey :: SessionKeyImpl -> PSessionT m ()
  setSessionKey newKey = do
    session <- getSession
    let updatedSession = session{ sessionKey = newKey }
    setSession updatedSession
    setKeyForSession (sessionKey session) newKey
    pure newKey

  --getUser :: PSessionT m SessionUserImpl
  getUser = sessionUser =<< getSession

  --getUserRequest :: PSessionT m RequestImpl
  getUserRequest = undefined

  --sendUserMessage :: PSessionT m ResponseImpl
  sendUserMessage = undefined

  -- The universal logging hook
  --logSession :: logMsg -> logLevel -> PSessionT m ()
  logSession msg _ = lift $ print $ "SERVER: " ++ msg

  -- Users can logout
  -- exitSession :: PSessionT m ()
  exitSession = getSession <* print "SERVER: Killing session" >>= terminateSession



newtype PSessionManagerT m a = PSessionManagerT (ReaderT SessionRecords m a) deriving (Monad)

runSessionManager action = do
  sessionRecords <- initSessionRecords
  runReaderT action sessionrecords

initSessionRecords = do
  sessions <- newIORef Map.empty
  let sessionCount = 0
  sessionMsgStack
  SessionRecords sessions sessionCount sessionReqStack

data SessionRecords
  = SessionRecords
    { sessions :: IORef (Map SessionKeyImpl (TVar SessionImpl))
    , sessionCount :: Int
    }

data SessionImpl =
  SessionImpl
    { sessionKey :: SessionKeyImpl
    , sessionUser :: SessionUserImpl
    , sessionMessageQueue :: [ResponseImpl]
    -- , sessionStartTime :: String
    }

newtype SessionCredentialsImpl = SessionCredentialsImpl { credentialsUsername :: String }

newtype SessionUserImpl = SessionUserImpl { username :: String }

newtype SessionKeyImpl = SessionKeyImpl { getSessionKey :: Int }

instance Monad m => SessionManager (PSessionManagerT m) where
  type Session (PSessionManagerT m) = SessionImpl
  type SessionCredentials (PSessionManagerT m) = SessionCredentialsImpl
  type SessionKey (PSessionManagerT m) = SessionKeyImpl
  type SesssionUser (PSessionManagerT m) = SessionUserImpl

  -- startAuthenticationUserSession :: SessionCredentialsImpl -> PSessionManagerT SessionImpl
  startAuthenticationUserSession SessionCredentialsImpl{..} = openSession credentialsUsername

  -- retrieveSession :: SessionKeyImpl -> PSessionManagerT (Maybe SessionImpl)
  retrieveSession sessionKey = do
    SessionRecords{..} <- ask
    pure $ Map.lookup sessions sessionKey

  ---- retrieveSessionUser :: SessionUserImpl -> PSessionManagerT SessionImpl
  --retrieveSessionUser SessionUserImpl{..} = undefined

  -- retrieveSessions :: PSessionManagerT [SessionImpl]
  retrieveSessions = do
    SessionRecords{..} <- ask
    pure $ map snd (Map.toList sessions)

  -- terminateSession :: SessionImpl -> PSessionManagerT ()
  terminateSession SessionImpl{..} = removeSession

openSession :: String -> PSessionManagerT SessionKeyImpl
openSession username = do
  SessionRecords{..} <- ask
  let sessionCount' = sessionCount + 1
  let sessionKey = SessionKeyImpl sessionCount'
  let sessionUser = SessionUserImpl username
  let session = SessionImpl sessionKey sessionUser
  let sessions' = Map.insert sessions session
  put $ SessionRecords (sessions') (sessionCount')
  pure sessionKey

removeSession :: SessionImpl -> PSessionManagerT ()
removeSession SessionImpl{..} = do
  SessionRecords{..} <- ask
  let sessions' = Map.delete sessions sessionKey
  put $ SessionRecords sessions' sessionCount

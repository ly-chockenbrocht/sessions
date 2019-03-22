module SessionImpl where


data RequestImpl = PrintString String | PringInt Int

type PSessionT m a = ReaderT (TVar SessionImpl) m a deriving (Monad)

runSession = runStateT

instance SessionManager m => AuthenticatedUserSession (PSessionT m) where
  type Request m = -- the sort of requests the type of session has
  type Response m -- the sort of responses we send to the user

  -- When we want to do something as a user within the user's session
  --withSession :: SessionImpl -> PSessionT m a -> m ()
  withSession = undefined

  -- Users should have access to getting and setting their session key... to some degree
  -- renewing should probably have a default implementation based on get and set

  getSession = lift . atomically . readTVar <=< ask


  -- setSession :: SessionImpl -> PSessionT m ()
  setSession session = do
    sessionVar <- ask
    lift . atomically $ writeTVar sessionVar session

  --renewSessionKey :: PSessionT m SessionKeyImpl
  renewSessionKey = setSessionKey =<< lift genSessionKey

  --getSessionKey :: PSessionT m SessionKeyImpl
  getSessionKey = do
    Session{..} <- getSession
    pure sessionKey

  --setSessionKey :: SessionKeyImpl -> PSessionT m ()
  setSessionKey newKey = do
    session <- getSession
    updatedSession = session{ sessionKey = newKey }
    setSession updatedSession
    pure newKey

  --getUser :: PSessionT m SessionUserImpl
  getUser = do
    Session{..} <- getSession
    pure sessionUser

  -- The universal logging hook
  --getUserRequest :: PSessionT m RequestImpl
  getUserRequest = undefined

  --sendUserResponse :: PSessionT m ResponseImpl
  sendUserResponse = undefined

  --logSession :: logMsg -> logLevel -> PSessionT m ()
  logSession = undefined

  -- Users can logout
  -- exitSession :: PSessionT m ()
  exitSession = undefined



type PSessionManagerT m a = StateT SessionRecords m a deriving (Monad)

data SessionRecords = SessionRecords { sessions :: Map SessionKeyImpl (TVar SessionImpl), sessionCount :: Int }

data SessionImpl =
  SessionImpl
    { sessionKey :: SessionKeyImpl
    , sessionUser :: SessionUserImpl
    -- , sessionStartTime :: String
    }

newtype SessionCredentialsImpl = SessionCredentialsImpl { credentialsUsername :: String }

newtype SessionUserImpl = SessionUserImpl { username :: String }

newtype SessionKeyImpl = SessionKeyImpl { getSessionKey :: Int }

instance Monad m=> SessionManager (PSessionManagerT m) where
  type Session (PSessionManagerT m) = SessionImpl
  type SessionCredentials (PSessionManagerT m) = SessionCredentialsImpl
  type SessionKey (PSessionManagerT m) = SessionKeyImpl
  type SesssionUser (PSessionManagerT m) = SessionUserImpl

  -- startAuthenticationUserSession :: SessionCredentialsImpl -> ProtoSession SessionImpl
  startAuthenticationUserSession SessionCredentialsImpl{..} = openSession credentialsUsername

  -- retrieveSession :: SessionKeyImpl -> ProtoSession (Maybe SessionImpl)
  retrieveSession sessionKey = do
    SessionRecords{..} <- ask
    case Map.lookup sessions sessionKey of
      Just s -> pure s
      Nothing -> error "real exception in production"

  ---- retrieveSessionUser :: SessionUserImpl -> ProtoSession SessionImpl
  --retrieveSessionUser SessionUserImpl{..} = undefined

  -- retrieveSessions :: ProtoSession [SessionImpl]
  retrieveSessions = do
    SessionRecords{..} <- ask
    pure $ map snd (Map.toList sessions)

  -- terminateSession :: SessionImpl -> ProtoSession ()
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

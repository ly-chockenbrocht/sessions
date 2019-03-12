module SessionImpl where

type ProtoSessionT m a = StateT SessionManager m a deriving (Monad)

data SessionManager = SessionManager { sessions :: Map SessionKeyImpl SessionImpl, sessionCount :: Int }

data SessionImpl =
  SessionImpl
    { sessionKey :: SessionKeyImpl
    , sessionUser :: SessionUserImpl
    -- , sessionStartTime :: String
    }

newtype SessionCredentialsImpl = SessionCredentialsImpl { credentialsUsername :: String }

newtype SessionUserImpl = SessionUserImpl { username :: String }

newtype SessionKeyImpl = SessionKeyImpl { getSessionKey :: Int }

openSession :: String -> ProtoSessionT SessionKeyImpl
openSession username = do
  SessionManager{..} <- ask
  let sessionCount' = sessionCount + 1
  let sessionKey = SessionKeyImpl sessionCount'
  let sessionUser = SessionUserImpl username
  let session = SessionImpl sessionKey sessionUser
  let sessions' = Map.insert sessions session
  put $ SessionManager (sessions') (sessionCount')
  pure sessionKey

removeSession :: SessionImpl -> ProtoSessionT ()
removeSession SessionImpl{..} = do
  SessionManager{..} <- ask
  let sessions' = Map.delete sessions sessionKey
  put $ SessionManager sessions' sessionCount

instance Monad m=> SessionMonad (ProtoSessionT m) where
  type Session (ProtoSessionT m) = SessionImpl
  type SessionCredentials (ProtoSessionT m) = SessionCredentialsImpl
  type SessionKey (ProtoSessionT m) = SessionKeyImpl
  type SesssionUser (ProtoSessionT m) = SessionUserImpl

  -- startAuthenticationUserSession :: SessionCredentialsImpl -> ProtoSession SessionImpl
  startAuthenticationUserSession SessionCredentialsImpl{..} = openSession credentialsUsername

  -- retrieveSession :: SessionKeyImpl -> ProtoSession (Maybe SessionImpl)
  retrieveSession sessionKey = do
    SessionManager{..} <- ask
    case Map.lookup sessions sessionKey of
      Just s -> pure s
      Nothing -> error "real exception in production"

  ---- retrieveSessionUser :: SessionUserImpl -> ProtoSession SessionImpl
  --retrieveSessionUser SessionUserImpl{..} = undefined

  -- retrieveSessions :: ProtoSession [SessionImpl]
  retrieveSessions = do
    SessionManager{..} <- ask
    pure $ map snd (Map.toList sessions)

  -- terminateSession :: SessionImpl -> ProtoSession ()
  terminateSession SessionImpl{..} = removeSession


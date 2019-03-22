module SessionMonad where

-- Ok, let's start with some rational for what we want withs some user stories
--
-- * user logs in
--
--   When a user logs in we want to initialize a session. We want to implement as a rule,
--   one session per user. So, we initialize the session at the first user login (first, as in,
--   never logged in, or previous session has expired).
--
--   So, we need a function which
--
-- * user reconnects
--
--   If a user is reconnecting, they are likely coming back in on a previous session, whether
--   active or expired. What we want to do then is to allow them to attempt to reinitialize the
--   session with a token, or if the token and session has expired, reauthenticate the user.
--
-- * user logs out
--
--   Maybe a conceivable action, more likely that not, not that important.
--
-- * admin boots user
--
--   An admin might boot a user on a few different actions:
--       * only a boot--maybe due to an inconsistent state
--       * admin locks a user account
--       * admin deletes/disables a user account permanently (is such a thing a thing?)
--

class Monad m => AuthenticatedUserSession m where
  type Request m -- the sort of requests the type of session has
  type Response m -- the sort of responses we send to the user

  -- When we want to do something as a user within the user's session
  getSession :: m (Session m)
  setSession :: (Session m) -> m ()

  -- Users should have access to getting and setting their session key... to some degree
  -- renewing should probably have a default implementation based on get and set
  renewSessionKey :: m (SessionKey m)
  getSessionKey :: m (SessionKey m)
  setSessionKey :: m (SessionKey m) -> m ()
  getUser :: m (SessionUser m)
  -- The universal logging hook
  getUserRequest :: m (Request m)
  sendUserResponse :: m (Response m)
  logSession :: Show logMsg => logMsg -> logLevel -> m ()
  -- Users can logout
  exitSession :: m ()


class Monad m => SessionManager m where
  type UserSession (Session m) => Session m
  type SessionCredentials m
  type SessionKey m
  type SessionUser m

  -- The only way to start a session should be to authenticate
  -- if a session exists, creating a session should be short-circuited

  genSessionKey :: m (SessionKey m)

  startAuthenticateUserSession :: SessionCredentials m -> m (Session m)

  -- We may want to look up a session
  retrieveSession :: SessionKey m -> m (Session m)

  -- We may want to retrieve all sessions
  retrieveSessions :: m [(Session m)]

  terminateSession :: Session m -> m ()

  terminateSessionByKey :: SessionKey m -> m ()
  terminateSessionByKey k = retrieveSession k >>= terminateSession

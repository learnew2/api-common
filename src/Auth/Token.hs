{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
module Auth.Token
  ( genericTokenFunctions
  , lookupToken
  , requireToken
  , requireRealmRoles
  , requireManyRealmRoles
  ) where

import           Api.Keycloak.Models
import qualified Api.Keycloak.Models.Introspect as I
import           Api.Keycloak.Models.Token
import           Api.Keycloak.Token
import           Api.Keycloak.Utils
import           Auth.Client
import           Control.Monad.Except
import           Control.Monad.Logger
import           Control.Monad.Reader
import           Data.Aeson                     (Value (..))
import           Data.Text
import           Models.JSONError
import           Servant.Client
import           Servant.Server
import           Service.Environment

lookupToken :: (MonadIO m, MonadLogger m, HasTokenVariable s Text, ServiceEnvironment s, MonadReader s m, MonadError ServerError m) => Text -> m I.IntrospectResponse
lookupToken token = do
  env <- asks $ getEnvFor AuthService
  withTokenVariable'' $ \t -> (liftIO . flip runClientM env) (postValidateRequest token (BearerWrapper t))

requireToken :: (MonadIO m, MonadLogger m, HasTokenVariable s Text, ServiceEnvironment s, MonadReader s m, MonadError ServerError m) => Text -> m I.IntrospectResponse
requireToken token = do
  r <- lookupToken token
  case r of
    I.InactiveToken -> sendJSONError err401 (JSONError "unauthorized" "Inactive token" Null)
    d@(I.ActiveToken {}) -> pure d

requireRealmRoles :: (MonadIO m, MonadLogger m, HasTokenVariable s Text, ServiceEnvironment s, MonadReader s m, MonadError ServerError m) => Text -> [Text] -> m I.IntrospectResponse
requireRealmRoles token roles = requireManyRealmRoles token [roles]

requireManyRealmRoles :: (MonadIO m, MonadLogger m, HasTokenVariable s Text, ServiceEnvironment s, MonadReader s m, MonadError ServerError m) => Text -> [[Text]] -> m I.IntrospectResponse
requireManyRealmRoles token rolesMap = do
  t <- requireToken token
  case t of
    ~(I.ActiveToken { .. }) -> do
      if Prelude.any (Prelude.all (`Prelude.elem` tokenRealmRoles)) rolesMap then
        pure t
      else sendJSONError err403 (JSONError "unauthorized" "Insufficent permissions" Null)

genericTokenFunctions :: (ServiceEnvironment t, MonadReader t m) => (Loc -> LogSource -> LogLevel -> LogStr -> IO ()) -> (Text, Text) -> m (TokenVariableFunctions Text)
genericTokenFunctions logF (cID, cSecret) = do
  env <- asks $ getEnvFor AuthService
  pure $ TokenFunctions
    { tokenValidateF=(\t -> runLoggingT (validate env t) logF)
    , tokenIssueF=runLoggingT (issue env) logF } where
  validate :: (MonadIO m, MonadLogger m) => ClientEnv -> Text -> m Bool
  validate env token = do
    r <- (liftIO . flip runClientM env) $ getTokenCapabilities (BearerWrapper token)
    case r of
      (Left e) -> do
        $(logDebug) $ pack $ "Failed to validate token: " <> show e
        return False
      (Right _) -> return True
  issue :: (MonadIO m, MonadLogger m) => ClientEnv -> m (Either String Text)
  issue env = do
    r <- (liftIO . flip runClientM env) $ postGrantRequest (ClientCredentialsRequest {reqClientSecret=cSecret, reqClientID=cID})
    case r of
      (Left e) -> do
        $(logError) $ pack $ "Failed to issue token: " <> show e
        (pure . Left . show) e
      (Right (GrantResponse {accessToken=accessToken})) -> (pure . pure) accessToken

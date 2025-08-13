{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
module Auth.Token
  ( genericTokenFunctions
  ) where

import           Api.Keycloak.Models
import           Api.Keycloak.Models.Token
import           Api.Keycloak.Token
import           Auth.Client
import           Control.Monad.IO.Class
import           Control.Monad.Logger
import           Data.Text
import           Servant.Client

genericTokenFunctions :: (MonadLogger m, MonadIO m) => (Text, Text) -> ClientEnv -> TokenVariableFunctions Text m
genericTokenFunctions (cID, cSecret) env = TokenFunctions {tokenValidateF=validate, tokenIssueF=issue} where
  validate :: (MonadIO m, MonadLogger m) => Text -> m Bool
  validate token = do
    r <- (liftIO . flip runClientM env) $ getTokenCapabilities (BearerWrapper token)
    case r of
      (Left e) -> do
        $(logDebug) $ pack $ "Failed to validate token: " <> show e
        return False
      (Right _) -> return True
  issue :: (MonadIO m, MonadLogger m) => m (Either String Text)
  issue = do
    r <- (liftIO . flip runClientM env) $ postGrantRequest (ClientCredentialsRequest {reqClientSecret=cSecret, reqClientID=cID})
    case r of
      (Left e) -> do
        $(logError) $ pack $ "Failed to issue token: " <> show e
        (pure . Left . show) e
      (Right (GrantResponse {accessToken=accessToken})) -> (pure . pure) accessToken

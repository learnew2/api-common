{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
module Service.Config
  ( requireEnv
  , requireKeycloakClient
  , requireEnvUrl
  ) where

import           Control.Monad.Catch
import           Control.Monad.IO.Class
import           Control.Monad.Logger
import           Data.Text              (Text, pack, unpack)
import           Servant.Client
import           System.Environment
import           System.Exit
import           Text.Read

requireEnv :: (Read a, MonadIO m) => String -> (Maybe String -> LoggingT m a) -> LoggingT m a
requireEnv envKey onFail = do
  v <- (liftIO . lookupEnv) envKey
  case v of
    Nothing -> onFail Nothing
    (Just v') -> case readMaybe v' of
      Nothing    -> onFail (Just v')
      (Just v'') -> pure v''

requireKeycloakClient :: (MonadIO m) => LoggingT m (Text, Text)
requireKeycloakClient = let
  f :: (MonadIO m) => t -> LoggingT m a
  f _ = do
    $(logError) "KEYCLOAK_CLIENT_SECRET or KEYCLOAK_CLIENT_ID is not provided"
    liftIO $ exitWith (ExitFailure 1)
  in do
    id' <- requireEnv "KEYCLOAK_CLIENT_ID" f
    secret' <- requireEnv "KEYCLOAK_CLIENT_SECRET" f
    return (id', secret')

requireEnvUrl :: (MonadIO m, MonadCatch m) => String -> LoggingT m BaseUrl
requireEnvUrl envKey = do
  rawUrl <- requireEnv envKey (\_ -> $(logError) ("Following URL is not provided: " <> pack envKey) >> (liftIO . exitWith) (ExitFailure 1))
  parseRes <- try $ parseBaseUrl rawUrl
  case parseRes of
    (Left (e :: SomeException)) -> do
      $(logError) ("Failed to parse " <> pack envKey <> " into URL: " <> (pack . show) e)
      (liftIO . exitWith) (ExitFailure 1)
    (Right url) -> pure url

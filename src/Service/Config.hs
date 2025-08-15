{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
module Service.Config
  ( requireEnv
  , requireKeycloakClient
  , requireEnvUrl
  , requireServiceEnv
  , lookupEnvDefault
  ) where

import           Control.Monad.Catch
import           Control.Monad.IO.Class
import           Control.Monad.Logger
import           Data.Functor                ((<&>))
import           Data.Text                   (Text, pack, unpack)
import           Network.HTTP.Client.Conduit
import           Servant.Client
import           Service.Ssl
import           System.Environment
import           System.Exit
import           Text.Read

lookupEnvDefault :: (Read a, MonadIO m) => String -> a -> LoggingT m a
lookupEnvDefault envKey def = do
  $(logDebug) $ "Looking for variable " <> pack envKey
  v <- (liftIO . lookupEnv) envKey
  case v of
    Nothing -> do
      $(logDebug) $ "Variable " <> pack envKey <> " not found. Returning default..."
      pure def
    (Just v') -> case readMaybe v' of
      Nothing    -> do
        $(logDebug) $ "Can't parse variable" <> pack envKey <> ". Returning default..."
        pure def
      (Just v'') -> pure v''

requireServiceEnv :: (MonadIO m, MonadCatch m) => String -> LoggingT m (BaseUrl, Manager)
requireServiceEnv prefix = let
  urlKey = prefix <> "_URL"
  sslKey = prefix <> "_IGNORE_SSL"

  in do
    ssl' <- lookupEnvDefault sslKey "0"
    url' <- requireEnvUrl urlKey
    mgr <- liftIO $ createSSLManager (ssl' == "1")
    return (url', mgr)

requireEnvText :: (MonadIO m) => String -> LoggingT m String -> LoggingT m Text
requireEnvText e f = requireEnv e f <&> pack

requireEnv :: (MonadIO m) => String -> LoggingT m String -> LoggingT m String
requireEnv envKey onFail = do
  $(logDebug) $ "Looking for variable " <> pack envKey
  v <- (liftIO . lookupEnv) envKey
  case v of
    Nothing -> do
      $(logDebug) $ "Variable " <> pack envKey <> " is not found."
      onFail
    (Just v') -> pure v'

requireEnvRead :: (Read a, MonadIO m) => String -> (Maybe String -> LoggingT m a) -> LoggingT m a
requireEnvRead envKey onFail = do
  $(logDebug) $ "Looking for variable " <> pack envKey
  v <- (liftIO . lookupEnv) envKey
  case v of
    Nothing -> do
      $(logDebug) $ "Variable " <> pack envKey <> " is not found."
      onFail Nothing
    (Just v') -> case readMaybe v' of
      Nothing    -> do
        $(logDebug) $ "Can't parse variable " <> pack envKey
        onFail (Just v')
      (Just v'') -> pure v''

requireKeycloakClient :: (MonadIO m) => LoggingT m (Text, Text)
requireKeycloakClient = let
  f :: (MonadIO m) => LoggingT m a
  f = do
    $(logError) "KEYCLOAK_CLIENT_SECRET or KEYCLOAK_CLIENT_ID is not provided"
    liftIO $ exitWith (ExitFailure 1)
  in do
    id' <- requireEnvText "KEYCLOAK_CLIENT_ID" f
    secret' <- requireEnvText "KEYCLOAK_CLIENT_SECRET" f
    return (id', secret')

requireEnvUrl :: (MonadIO m, MonadCatch m) => String -> LoggingT m BaseUrl
requireEnvUrl envKey = do
  rawUrl <- requireEnv envKey ($(logError) ("Following URL is not provided: " <> pack envKey) >> (liftIO . exitWith) (ExitFailure 1))
  parseRes <- try $ parseBaseUrl rawUrl
  case parseRes of
    (Left (e :: SomeException)) -> do
      $(logError) ("Failed to parse " <> pack envKey <> " into URL: " <> (pack . show) e)
      (liftIO . exitWith) (ExitFailure 1)
    (Right url) -> pure url

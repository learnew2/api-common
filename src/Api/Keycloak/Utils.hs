{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module Api.Keycloak.Utils
  ( unwrapError
  , withTokenVariable'
  , withTokenVariable''
  ) where

import           Api.Keycloak.Token
import           Control.Monad.Except
import           Control.Monad.Logger
import           Control.Monad.Reader
import           Data.Aeson
import           Data.Text
import           Models.JSONError
import           Servant

unwrapError :: (MonadError ServerError m, MonadLogger m, Show b) => Either b a -> m a
unwrapError (Left e) = do
  $(logDebug) $ "Unwrapped following error: " <> (pack . show) e
  sendJSONError err500 (JSONError "internalError" "Internal server error" Null)
unwrapError (Right a) = pure a

withTokenVariable' :: (MonadIO m, HasTokenVariable t, MonadReader t m, MonadError ServerError m, MonadLogger m) => (a -> m b) -> m b
withTokenVariable' f = withTokenVariable f >>= unwrapError

withTokenVariable'' :: (MonadIO m, HasTokenVariable t, MonadReader t m, MonadError ServerError m, MonadLogger m, Show e) => (a -> m (Either e b)) -> m b
withTokenVariable'' f = withTokenVariable' f >>= unwrapError

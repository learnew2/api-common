{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Models.JSONError
  ( JSONError(..)
  , sendJSONError
  ) where

import           Control.Monad.Except
import           Data.Aeson
import           Servant

data JSONError = JSONError
  { errorType    :: !String
  , errorMessage :: !String
  , errorContext :: !Value
  } deriving Show

instance ToJSON JSONError where
  toJSON (JSONError { .. }) = object
    [ "type" .= errorType
    , "error" .= errorMessage
    , "context" .= errorContext
    ]

sendJSONError :: (MonadError ServerError m) => ServerError -> JSONError -> m a
sendJSONError err body = throwError $ err { errBody = encode body }

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Models.JSONError
  ( JSONError(..)
  ) where

import           Data.Aeson

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

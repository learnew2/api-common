{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeOperators     #-}
module Auth.Schema
  ( AuthAPI
  ) where

import           Api
import           Api.Keycloak.Models.Introspect
import           Api.Keycloak.Models.Role
import           Api.Keycloak.Models.Token
import           Data.Text
import           Servant.API

type AuthAPI = "api" :> "auth" :> ReqBody '[JSON] GrantRequest :> Post '[JSON] GrantResponse
  :<|> "api" :> "auth" :> "validate" :> ReqBody '[JSON] Text :> AuthHeader :> Post '[JSON] IntrospectResponse
  :<|> "api" :> "auth" :> "roles" :> AuthHeader :> Get '[JSON] [RealmRole]
  :<|> "api" :> "auth" :> "roles" :> AuthHeader :> ReqBody '[JSON] RoleCreateRequest :> Post '[JSON] ()
  :<|> "api" :> "auth" :> "roles" :> AuthHeader :> Capture "RoleName" Text :> Delete '[JSON] ()
  :<|> "api" :> "auth" :> "capabilities" :> AuthHeader :> Get '[JSON] [Text]
  :<|> "api" :> "auth" :> "login" :> QueryParam "redirectTo" Text :> Get '[JSON] ()
  :<|> "api" :> "auth" :> "logout" :> Get '[JSON] ()
  :<|> "api" :> "auth" :> "fail" :> Get '[JSON] ()
  :<|> "api" :> "auth" :> "callback" :> QueryParam "code" Text :> QueryParam "state" Text :> Get '[JSON] ()

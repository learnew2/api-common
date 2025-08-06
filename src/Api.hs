{-# LANGUAGE DataKinds #-}
module Api where

import           Api.Keycloak.Models
import           Servant.API

type AuthHeader = Header' '[Required] "Authorization" BearerWrapper

{-# LANGUAGE DataKinds #-}
module Deployment.Client
  ( getPagedTemplates
  , deleteTemplate
  , createTemplate
  , getPagedDeploymentTemplates
  , createDeploymentTemplate
  , getDeploymentTemplate
  , deleteDeploymentTemplate
  , patchDeploymentTemplate
  , callGroupDeployment
  , requestDeploymentVMID
  , requestDeploymentDisplay
  , getDeploymentTemplateInstances
  , getMyTemplateInstances
  , getDeploymentInstance
  , getVMPortPower
  , switchVMPortPower
  , getVMPortNetworks
  ) where

import           Data.Proxy
import           Deployment.Schema
import           Servant
import           Servant.Client

api :: Proxy DeploymentAPI
api = Proxy

getPagedTemplates
  :<|> deleteTemplate
  :<|> createTemplate
  :<|> getPagedDeploymentTemplates
  :<|> createDeploymentTemplate
  :<|> getDeploymentTemplate
  :<|> deleteDeploymentTemplate
  :<|> patchDeploymentTemplate
  :<|> callGroupDeployment
  :<|> requestDeploymentVMID
  :<|> requestDeploymentDisplay
  :<|> getDeploymentTemplateInstances
  :<|> getMyTemplateInstances
  :<|> getDeploymentInstance
  :<|> getVMPortPower
  :<|> switchVMPortPower
  :<|> getVMPortNetworks
  :<|> _ = client api

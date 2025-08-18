{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeOperators     #-}
module Deployment.Schema
--  ( DeploymentAPI
  (
  ) where

import           Api
import           Cluster.Models.Node
import           Data.Text
import           Proxmox.Deploy.Models.Config.Template
import           Servant.API

type TemplateNameCapture = Capture "TemplateName" Text
type DeploymentTemplateCapture = Capture "DeploymentTemplateID" Int
type NodeNameCapture = Capture "NodeName" Text

--type DeploymentAPI = "api" :> "deployment" :> "templates" :> QueryParam "page" Int :> AuthHeader :> Get '[JSON] (PagedResponse [ConfigTemplate])
--  :<|> "api" :> "deployment" :> "templates" :> TemplateNameCapture :> AuthHeader :> Delete '[JSON] ()
--  :<|> "api" :> "deployment" :> "templates" :> ReqBody '[JSON] ConfigTemplate :> AuthHeader :> Post '[JSON] ()
--  :<|> "api" :> "deployment" :> "deployments" :> ReqBody '[JSON] DeploymentCreate :> AuthHeader :> Post '[JSON] ()
--  :<|> "api" :> "deployment" :> "deployments" :> DeploymentTemplateCapture :> AuthHeader :> Get '[JSON] DeploymentTemplate
--  :<|> "api" :> "deployment" :> "deployments" :> DeploymentTemplateCapture :> AuthHeader :> Delete '[JSON] ()
--  :<|> "api" :> "deployment" :> "deployments" :> DeploymentTemplateCapture :> ReqBody '[JSON] DeploymentCreate :> AuthHeader :> Patch '[JSON] ()
--  :<|> "api" :> "deployment" :> "vmid" :> NodeNameCapture :> QueryParam "amount" Int :> AuthHeader :> Get '[JSON] [Int]
--  :<|> "api" :> "deployment" :> "display" :> NodeNameCapture :> QueryParam "amount" Int :> AuthHeader :> Get '[JSON] [Int]
--  :<|> "api" :> "deployment" :> "deployments" :> DeploymentTemplateCapture :> "instances" :> QueryParam "page" Int :> AuthHeader :> Get '[JSON] (PagedResponse [DeploymentInstance])
--  :<|> "api" :> "deployment" :> "instances" :> "my" :> QueryParam "page" Int :> AuthHeader :> Get '[JSON] (PagedResponse [DeploymentInstance])

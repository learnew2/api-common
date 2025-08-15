module Cluster.Client
  ( getPagedNodes
  , getNodeByName
  , createNode
  , deleteNodeByName
  , getFreeNetworkName
  , getDeployNode
  , getWebsockifyConfig
  ) where

import           Cluster.Schema
import           Servant
import           Servant.Client

api :: Proxy ClusterManagerAPI
api = Proxy

getPagedNodes
  :<|> getNodeByName
  :<|> createNode
  :<|> deleteNodeByName
  :<|> getFreeNetworkName
  :<|> getDeployNode
  :<|> getWebsockifyConfig = client api

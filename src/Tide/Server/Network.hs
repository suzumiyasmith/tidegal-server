{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module Tide.Server.Network where


import Control.Monad
import Control.Arrow
import Data.Monoid
import Data.Foldable

import qualified Data.ByteString.Lazy as BL
import qualified Data.Binary as B

import Control.Concurrent
import Control.Concurrent.Chan as C

import Network.Wai (Application, responseLBS)
import Network.Wai.Handler.Warp
import Network.Wai.Handler.WarpTLS
import Network.Wai.Handler.WebSockets
import Network.WebSockets

import Network.HTTP.Types (status400)

import Tide.Server.Types
import Tide.Types
import Tide.Server.Init

mainServer = do
  s :: SS <- setServer
  forever $ do
    (u1, c1) <- C.readChan s
    (u2, c2) <- C.readChan s
    s1 <- toChans c1
    s2 <- toChans c2
    tid <- forkIO $ mainGame (s1,s2)
    print $ show tid <> " start a new game"

toChans conn = do
  i <- newEmptyMVar
  r <- C.newChan
  forkIO $ forever $ do
    ii :: PlayerInput <-  B.decode <$> receiveData conn
    print $ "get" <> show ii
    putMVar i ii
  forkIO $ forever $ do
    rr :: DisplayData <- C.readChan r
    sendBinaryData conn $ B.encode rr
  return (i, r)

setServer :: IO SS
setServer = do
  s :: SS <- C.newChan
  -- forkIO $ runTLS mytls mySettings $ buildConnection s
  forkIO $ run 8888 $ buildConnection s
  return s

mySettings :: Settings
mySettings = setPort 443 defaultSettings

mytls :: TLSSettings
mytls = tlsSettingsChain certPath [chainPath, fullChainPath] keyPath where
  store = "/etc/letsencrypt/live/fff.prpr.link/"
  certPath = store <> "cert.pem"
  keyPath = store <> "privkey.pem"
  chainPath = store <> "chain.pem"
  fullChainPath = store <> "fullchain.pem"

buildConnection :: SS -> Application
buildConnection s = websocketsOr defaultConnectionOptions wsApp backupApp
  where
    wsApp :: ServerApp
    wsApp pending_conn = do
      print $ pendingRequest pending_conn
      case checkUserInfo $ requestHeaders $ pendingRequest pending_conn of
        Left e ->
          print e
        Right (UserInfo uname _) -> do
          conn <- acceptRequest pending_conn
          C.writeChan s (uname, conn)
          forever $ threadDelay 10000

    backupApp :: Application
    backupApp _ respond = respond $ responseLBS status400 [] ("Not a WebSocket request" :: BL.ByteString)

checkUserInfo :: Headers -> Either String UserInfo
checkUserInfo hs =
  case find ((== "User-Info") . fst) hs of
    Nothing -> Left "User Info not found"
    Just h -> Right $ B.decode $ BL.fromStrict $ snd h

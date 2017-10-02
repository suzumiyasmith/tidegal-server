{-# LANGUAGE ScopedTypeVariables #-}

module Tide.Server.Init where

import Apecs as A
import Apecs.Concurrent
import Linear
import Control.Lens
import Control.Monad
import Control.Arrow
import Control.Monad.IO.Class
import Control.Concurrent
import Data.Monoid
import Data.Foldable
import qualified Data.Set as S

import Physics.Light
import Tide.Types
import Tide.Server.Types

mainGame :: ((MVar PlayerInput, Chan DisplayData), (MVar PlayerInput, Chan DisplayData)) -> IO ()
mainGame ((usr1I,usr1O) , (usr2I,usr2O)) = do
  world <- initGameWorld
  let
    gameLogic = do
      newEntity (playerObject & position .~ (V3 0 (-25) 0), Player1, Team 1, Controller usr1I, initPlayerState, ShipState [])
      newEntity (cameraObject & position .~ V3 0 0 0, Camera usr1O)
      newEntity (playerObject & position .~ (V3 0 25 pi), Player2, Team 2, Controller usr2I, initPlayerState, ShipState [])
      newEntity (cameraObject & position .~ V3 0 0 pi, Camera usr2O)
      -- concurrently
      --   [ periodic 1000 $ do
      --       cimapM_ controlPlayer
      --       cmapM effectExec
      --       cmap effectRemove
      --       physicsUpdate 0.001
      --   , periodic 16666 $ cmapM_ render
      --   ]
      periodic 16666 $ do
        cimapM_ controlPlayer
        cmapM effectExec
        cmap effectRemove
        physicsUpdate 0.016666
        cmapM_ render

      return ()
  runSystem gameLogic world
  return ()

initPlayerState = PlayerState S.empty (replicate 400 CardFireBall,replicate 9 CardIceBall)

-- forkSystem :: System w () -> System w ()
-- forkSystem sys = do
--   w <- System ask
--   void . liftIO . forkIO $ runSystem sys w

render :: (Object2D, Camera) -> System GameWorld ()
render (o, Camera c) = do
  let p = o ^. position
  [pos1] <- cmapM (\(p1, Player1) -> return (p1 ^. position))
  [pos2] <- cmapM (\(p2, Player2) -> return (p2 ^. position))
  bs <- cmapM (\(b, Bullet) -> return (b ^. position))
  liftIO $ do
    let d = (DisplayData (worldToLocal p pos1) (worldToLocal p pos2) (worldToLocal p <$> filter ((<50) . norm) bs))
    writeChan c d

effectExec :: (Object2D, ShipState) -> System GameWorld ()
effectExec (o, st) = do
  let
    effectLauncher :: Effect -> System GameWorld ()
    effectLauncher eff = case eff of
        Fire b -> do
          newEntity (bulletObject (o^.position) (o^.velocity), Bullet)
          return ()
        _ -> return ()
  traverse_ effectLauncher (st ^. effectQueue)

effectRemove :: ShipState -> ShipState
effectRemove = effectQueue .~ []

controlPlayer :: (Entity c, Controller) -> System GameWorld ()
controlPlayer (ety, (Controller input)) = do
  cc <- liftIO $ tryTakeMVar input
  case cc of
    Nothing -> return ()
    Just c -> do
      A.modify (cast ety) (stateChange c)

stateChange :: PlayerInput -> (PlayerState, ShipState) -> (PlayerState, ShipState)
stateChange c (pST, sST) = (pST', sST & effectQueue %~ effectChange) where
  pST' = pST & moveState %~ updateMoveState c & fireState .~ fst fST
  effectChange = case snd fST of
           Nothing -> id
           Just b -> (b :)
  fST = fireChange c (pST ^. fireState)

updateMoveState :: PlayerInput -> S.Set MoveDirection -> S.Set MoveDirection
updateMoveState c = if snd c then S.insert (fromCommand (fst c)) else S.delete (fromCommand (fst c))

fireChange :: PlayerInput -> ([Card], [Card]) -> (([Card], [Card]), Maybe Effect)
fireChange c fs =
  case c of
    (MoveLeft, True) -> (autoChange $ goLeft fs, Nothing)
    (MoveRight, True) -> (autoChange $ goRight fs, Nothing)
    (MoveUp, True) -> first autoChange $ launchMiddle fs
    _ -> (fs, Nothing)
  where
    goRight (l, r : rs) = (r : l, rs)
    goRight (l, []) = (l, [])
    goLeft (l : ls, rs) = (ls, l : rs)
    goLeft ([], r) = ([], r)
    launchMiddle (l, r : rs) = ((l, rs), Just $ Fire r)
    launchMiddle (l, []) = ((l, []), Nothing)
    autoChange (l:ls, []) = (ls,[l])
    autoChange (l,r) = (l,r)

-- in micro sec (10^(-6) sec)
periodic :: MonadIO m => Int -> m () -> m ()
periodic dt sys = do
  t <- liftIO $ newMVar ()
  liftIO $ forkIO $ forever $ threadDelay dt >> putMVar t ()
  forever $ liftIO (takeMVar t) >> sys

physicsUpdate :: Double -> System GameWorld ()
physicsUpdate dt = do
  A.rmap (engineUpdate dt)
  -- A.rmap (airResistence dt)
  A.rmap (updateObject2D dt)

engineUpdate dt (o, pST) =
  o & velocity %~ (\v -> v + dt *^ engineForce)
  where
    engineDirection b =
      case b of
        MovingUp -> V3 0 10 0
        MovingDown -> V3 0 (-10) 0
        MovingLeft -> V3 (-10) 0 0
        MovingRight -> V3 10 0 0
    engineForce = sum (fmap (engineDirection >>> _xy %~ (rotateMatrix2D (o ^. position . _z) !*)) (S.toList $ pST ^. moveState))

airResistence dt o =
  o & velocity %~ (\v -> v - dt * 0.1 * norm v *^ v)

fromCommand c = case c of
  MoveUp -> MovingUp
  MoveDown -> MovingDown
  MoveLeft -> MovingLeft
  MoveRight -> MovingRight

playerObject = Object2D
  { _position = 0
  , _velocity = 0
  , _shape = [circle 1]
  }

cameraObject = Object2D
  { _position = 0
  , _velocity = 0
  , _shape = []
  }

bulletObject p v = Object2D
  { _position = p
  , _velocity = v
  , _shape = [circle 1]
  }

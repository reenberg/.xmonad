{-# LANGUAGE TypeSynonymInstances #-} -- For: instance HasColorizer WindowSpace

-- Original Work by Morten Brøns
module XMonad.Stack.MyGridSelect
       (
         myShiftToSelectedWS
       , myGoToSelectedWS
       , myGoToSelected
       )
       where

import Data.List (partition)
import Control.Monad (liftM2)


import XMonad

-- GridSelect displays items(e.g. the opened windows) in a 2D grid and lets the
-- user select from it with the cursor/hjkl keys or the mouse.
import XMonad.Actions.GridSelect
  ( HasColorizer(..), GSConfig(..)
  , defaultGSConfig, stringColorizer, gridselect
  , navNSearch, goToSelected
  )

-- Turns your workspaces into a more topic oriented system.
import XMonad.Actions.TopicSpace (switchTopic)

-- The StackSet data type encodes a window manager abstraction.
import qualified XMonad.StackSet as W


-- My custom Topics
import XMonad.Stack.MyTopics (myTopicConfig)


myGSConfig :: HasColorizer a => GSConfig a
myGSConfig = defaultGSConfig {gs_navigate = navNSearch}


myShiftToSelectedWS inclEmpty = shiftToSelectedWS inclEmpty myGSConfig
myGoToSelectedWS inclEmpty = goToSelectedWS inclEmpty myGSConfig

myGoToSelected = goToSelected myGSConfig

nonEmptyWS :: WindowSpace -> Bool
nonEmptyWS = (/= Nothing) . W.stack

-- Brings up a 2D GridSelect menu of WorkSpaces. If inclEmpty is True, the shown
-- list also include empty WorkSpaces. The chosen WorkSpace is returned (if any
-- was chosen).
gridselectWS :: Bool -> GSConfig WindowSpace -> X (Maybe WindowSpace)
gridselectWS inclEmpty conf =
  -- Run a monadic action with the current stack set
  -- ws (WindowSpace) is a StackSet
  withWindowSet $ \ws -> do
    let -- Get all hidden Workspaces
        hid = W.hidden ws
        -- Get Workspace from list of visible screens.
        vis = map W.workspace $ W.visible ws
        -- All Workspaces (except the one visible on the current screen)
        all = hid ++ vis
        (nonEmp, emp) = partition nonEmptyWS all
        wss = if inclEmpty
              then nonEmp ++ emp
              else nonEmp
        -- Create list of (tag, workspace) as these are used as the "heading" in
        -- the GridSelect menu
        namedWss = map (\workS -> (W.tag workS, workS)) wss
    gridselect conf namedWss


-- Run the callback function on the selected WorkSpace from the GridSelect menu.
withSelectedWS :: (WindowSpace -> X ()) -> Bool -> GSConfig WindowSpace -> X ()
withSelectedWS callback inclEmpty conf = do
  selectedWS <- gridselectWS inclEmpty conf
  case selectedWS of
    Just ws -> callback ws
    Nothing -> return ()



-- Specialised version of withSelectedWS, that switches to the chosen WorkSpace
goToSelectedWS :: Bool -> GSConfig WindowSpace -> X ()
goToSelectedWS = withSelectedWS $ switchTopic myTopicConfig . W.tag


-- Specialised version of withSelectedWS, that moves the current Window to the
-- selected Workspace.
shiftToSelectedWS :: Bool -> GSConfig WindowSpace -> X ()
shiftToSelectedWS =
  -- (windows) Modify the current window list with a pure function, and refresh
  withSelectedWS $ windows . (\ws -> W.greedyView ws . W.shift ws) . W.tag


instance HasColorizer WindowSpace where
  defaultColorizer ws isFg =
    if nonEmptyWS ws || isFg
    then stringColorizer (W.tag ws) isFg
         -- Empty workspaces get a dusty-sandy-ish colour
    else return ("#CAC3BA", "white")

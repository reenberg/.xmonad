module XMonad.Stack.MyConfig
       (
         myConfig
       )
       where


import XMonad

-- This module provides a config suitable for use with the KDE desktop
-- environment.
import XMonad.Config.Kde (kde4Config)

import XMonad.ManageHook (liftX)

-- Make a given layout display without borders. This is useful for full-screen
-- or tabbed layouts, where you don't really want to waste a couple of pixels of
-- real estate just to inform yourself that the visible window has focus.
import XMonad.Layout.NoBorders (smartBorders)
-- This is a layout modifier that will show the workspace name.
import XMonad.Layout.ShowWName (showWName)

-- This module provides tools to automatically manage dock type programs, such
-- as gnome-panel, kicker, dzen, and xmobar.
import XMonad.Hooks.ManageDocks (manageDocks, avoidStrutsOn)
-- Makes XMonad set the _NET_WM_WINDOW_OPACITY atom for inactive windows, which
-- causes those windows to become slightly translucent if something like
-- xcompmgr is running.
import XMonad.Hooks.FadeInactive (fadeInactiveCurrentWSLogHook, fadeInactiveLogHook)

-- Used for kdeOverride
import XMonad.Util.WindowProperties (getProp32s)
-- Miscellaneous commonly used types. Direction2D exports: U, D, L, R
import XMonad.Util.Types (Direction2D(..))

-- Used for setting the lowest logging level.
import System.Log.Logger (Priority(..))

-- Temporarily fix SWING applications
import XMonad.Hooks.SetWMName (setWMName)


-- My modifications
import XMonad.Stack.MyKeys (myKeys)
import XMonad.Stack.MyLayout (myLayout)
import XMonad.Stack.MyTopics (myTopics, myManageHook)


myDesktopConfig = kde4Config

kdeOverride :: Query Bool
kdeOverride = ask >>= \w -> liftX $ do
  override <- getAtom "_KDE_NET_WM_WINDOW_TYPE_OVERRIDE"
  wt <- getProp32s "_NET_WM_WINDOW_TYPE" w
  return $ maybe False (elem $ fromIntegral override) wt

myConfig =
  myDesktopConfig
    { -- Use win key instead of alt key as modifier.
      -- Doesn't conflict emacs.
      modMask = mod4Mask -- default is mod1Mask

      -- Do the right thing, when certain programs start.
      -- We have some kde defautls and our own myMangeHook
    , manageHook =
        ((className =? "krunner") >>= return . not --> manageHook kde4Config
          <+> (kdeOverride --> doFloat))
        <+> manageDocks
        <+> composeAll myManageHook

    , layoutHook =
        -- Makes room for panel.
        avoidStrutsOn [D] $
        -- make floats float.
        smartBorders $
        -- Show workspace name.
        showWName $
        -- Set CWD according to the current workspace.
        myLayout

      -- Disable (red) borders
    , borderWidth = 0

      -- Key bindings
    , keys = myKeys myDesktopConfig

      -- Dont change focus when mouse hovers
    , focusFollowsMouse = False

    , logHook = do
        -- Sets transparancy on non focues windows.
        fadeInactiveCurrentWSLogHook 0.7
        -- fadeInactiveLogHook 1

        -- Fix SWING applications.
        >> setWMName "LG3D"

    , workspaces = myTopics

    , logFilePriority = DEBUG
    }

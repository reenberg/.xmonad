module XMonad.Stack.MyKeys
       (
         myKeys
       )
       where


import qualified Data.Map as M

import XMonad

-- Custom GridSelect implementation that shows WorkSpaces
import XMonad.Stack.MyGridSelect
  (myShiftToSelectedWS, myGoToSelectedWS, myGoToSelected)

type KeyActionMap = M.Map (ButtonMask, KeySym) (X ())

myKeys myDesktopConfig conf@(XConfig {modMask = modM}) =
  myNewKeys modM `M.union` myDesktopConfigKeys
    where
      desktopConfigKeys = keys myDesktopConfig conf
      myDesktopConfigKeys =
        M.foldrWithKey (changeConfigKeys modM) M.empty desktopConfigKeys

myNewKeys :: ButtonMask -> KeyActionMap
myNewKeys modM =
  M.fromList
  [ -- Show all windows, and go to selected
    ((modM,  xK_g), myGoToSelected)

    -- Show non-empty workspaces
  , ((modM,  xK_z), myGoToSelectedWS False)
    -- Show non-empty workspaces, and move current window to selected
  , ((modSM, xK_z), myShiftToSelectedWS False)

    -- Show all workspaces
  , ((modM,  xK_a), myGoToSelectedWS True)
    -- Show all workspaces, and move current window to selected
  , ((modSM, xK_a), myShiftToSelectedWS True)
  ]
    where
      modSM = modM .|. shiftMask


-- | Allows for filtering and easy re-binding of keys and actions, by setting up
-- you any combination of guards to modify the resulting keyActionmap
changeConfigKeys :: ButtonMask -> (ButtonMask, KeySym) -> X() -> KeyActionMap -> KeyActionMap
changeConfigKeys modM keys action newConfigKeys
    -- Keep the ones below, in unmodified state.
  | keys `elem`
    [ -- Reload XMonad
      (modM,  xK_q)
      -- Quit XMonad
    , (modSM, xK_q)

      -- Launch terminal
    , (modSM, xK_Return)

    -- Close the focused window
    , (modSM, xK_c)

    -- Rotate through the available layout algorithms
    , (modM,  xK_space)
    -- Reset the layouts on the current workspace to default
    , (modSM, xK_space)

    -- Resize viewed windows to the correct size
    , (modM,  xK_n)

    -- Move focus to the next window
    , (modM,  xK_Tab)
    -- Move focus to the previous window
    , (modSM, xK_Tab)

    -- Move focus to the next window
     , (modM,  xK_j)
    -- Move focus to the previous window
    , (modM,  xK_k)
    -- Move focus to the master window
    , (modM,  xK_m)
    -- Swap the focused window and the master window
    , (modM,  xK_Return)

    -- Swap the focused window with the next window
    , (modSM, xK_j)
    -- Swap the focused window with the previous window
    , (modSM, xK_k)

    -- Shrink the master area
    , (modM,  xK_h)
    -- Expand the master area
    , (modM,  xK_l)

    -- Push window back into tiling
    , (modM,  xK_t)

    -- Increment the number of windows in the master area
    , (modM,  xK_comma)
    -- Decrement the number of windows in the master area
    , (modM,  xK_period)

    -- Toggle the status bar gap
    , (modM,  xK_b)

    -- Switch to physical/Xinerama screens 1, 2, or 3
    , (modM,  xK_w)
    , (modM,  xK_e)
    , (modM,  xK_r)

    -- Move client to screen 1, 2, or 3
    , (modSM, xK_w)
    , (modSM, xK_e)
    , (modSM, xK_r)
    ] = M.insert keys action newConfigKeys

     -- Default, remove the key/action binding
   | otherwise = newConfigKeys
  where
    modSM = modM .|. shiftMask

module XMonad.Stack.MyLayout
       (
         myLayout
       )
       where


-- The collection of core layouts.
import XMonad.Layout (Tall(..), (|||))
-- A tabbed layout for the Xmonad Window Manager
import XMonad.Layout.Tabbed (simpleTabbedBottom)


-- Possible layouts.
myLayout =
  Tall
    1       -- The default number of windows in the master pane (default: 1)
    (3/100) -- Percent of screen to increment by when resizing panes (default: 3/100)
    (5/7)   -- Default proportion of screen occupied by master pane (default: 1/2)
  |||
  simpleTabbedBottom -- A bottom-tabbed layout with the default xmonad Theme.

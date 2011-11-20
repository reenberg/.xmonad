{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TypeSynonymInstances #-}


-- Default stuff used.
import Data.Map (unions, fromList)
import Data.List (partition)
import Control.Monad (liftM2)


import XMonad

-- Enable topic related namespaces with hooks. The hooks are run if the
-- workspaces are empty
import XMonad.Actions.TopicSpace (checkTopicConfig)


-- My Costrom XMonad
import XMonad.Stack.MyTopics (myTopics, myTopicConfig)
import XMonad.Stack.MyConfig (myConfig)


main = do
  -- Verify topics based workspaces. This is however not needed anymore, as of
  -- my new design.
  checkTopicConfig myTopics myTopicConfig
  xmonad $ myConfig





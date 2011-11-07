module XMonad.Stack.MyTopics
       ( myTopics
       , myTopicConfig
       , myManageHook
       )
       where

import qualified Data.Map as M
import Control.Monad (liftM2)


import XMonad

-- The StackSet data type encodes a window manager abstraction.
import qualified XMonad.StackSet as W

-- Turns your workspaces into a more topic oriented system.
import XMonad.Actions.TopicSpace (TopicConfig(..))

-- To find the property name associated with a program, use
-- > xprop | grep WM_CLASS
-- and click on the client you're interested in.

-- For XMonadContrib (>= 0.10, from darcs), look in
-- $src/XMonadContrib/scripts
-- xprop script that will print window information (if available) in formats
-- usable within xmonad.hs.

-- X properties:
-- * resource (also known as appName) is the first element in WM_CLASS(STRING)
-- * className is the second element in WM_CLASS(STRING)
-- * title is WM_NAME(STRING)

{-  "Topic name"
  , empty desktop action
  , [List of manage hooks: (Query , ManageHook)]
-}

-- TODO: doIgnore takes no arguments, so make a custom one that is wrapped
-- inside `const`, such that it can take the topic as argument anyway.

myTopicsDef =
  [ ( "Web" -- The first one is the default.
    , spawn "chromium-browser"
    , [])

  , ( "Instant messaging"
    , spawn "kopete" >> -- kde msn/jabber
      spawn "quassel"   -- kde irc
    , [ (className =? "Kopete" <||>
         appName =? "kopete", doShift)
      , (className =? "Quassel", doShift)])

  , ( "Multimedia"
    , spawn $ inDummyBrowser "http://grooveshark.com"
    , [(className =? "Sonata", doShift)])

  , ( "Organise"
    , spawn "gmail" >>
      spawn "gcal"
    , [])

  , ( "Reading"
    , return ()
    , [])


  , ( "Study"
    , return ()
    , [])

  , ( "Absalon"
    , spawn $ inDummyBrowser "punkt.ku.dk"
    , [])

  , ( "VirtualBox"
    , spawn "virtualbox"
    , [])

  , ( "Download"
    , spawn "ktorrent"
    , [(className =? "Ktorrent", doShift)])


    -- Configuration
  , ( "XMonad"
    , spawn "emacs ~/.xmonad/xmonad.hs" >>
      spawn (inDummyBrowser "http://xmonad.org/xmonad-docs/xmonad-contrib/index.html")
    , [])

  , ( "Install"
    , spawn "konsole"
    , [])

    -- Coding
  , ( "SML"
    , return ()
    , [])

  , ( "haskell"
    , return ()
    , [])

    -- Projects
  , ("speciale"
    , return ()
    , [])
  ]
    where
      -- As long as it aint used, we comment it out such that ghc won't complain
      -- about type variables
      -- Opens on view, and makes that view the current active one.
      --viewShift = doF . liftM2 (.) W.greedyView W.shift
      inBrowser s = "chromium-browser " ++ s
      inDummyBrowser s = "surf " ++ s

myDefaultTopicAction = spawn "konsole"

myTopics = map getTopic myTopicsDef

myTopicsAction = map (\(topic, action, _) -> (topic, action)) myTopicsDef

myManageHook = (concat $ unfoldTopicHooks)
               -- Debugging hooks
               --++ myDebugManageHook
  where
    nonEmptyHooks = filter (not . null . getManageHooks) myTopicsDef
    unfoldHooks topic =
      map (\(query, manageHook) -> query --> -- emacs thinks the arrow is a comment
                                   manageHook topic )
    unfoldTopicHooks =
      map (\(topic, _, manageHooks) -> unfoldHooks topic manageHooks) nonEmptyHooks


myDebugManageHook =
  [return True -->
   do
     title     >>= \t   -> debugOutputLn $ "Title: '" ++ t ++ "'"
     appName   >>= \an  -> debugOutputLn $ "AppName: '" ++ an  ++ "'"
     resource  >>= \res -> debugOutputLn $ "Resource: '" ++ res  ++ "'"
     className >>= \cn  -> debugOutputLn $ "ClassName: '" ++ cn  ++ "'"
     debugOutputLn "-------------------"
     idHook
  ]
    where
      debugOutputLn s = io $ appendFile "/tmp/xmonad_debug" (s ++ "\n")



myTopicConfig = TopicConfig
  { topicDirs = M.empty
  , topicActions = M.fromList myTopicsAction
  , defaultTopicAction = const myDefaultTopicAction
  , defaultTopic = getTopic $ head myTopicsDef
  , maxTopicHistory = 0 -- meh ?
  }



getTopic (topic, _, _) = topic
getAction (_, action, _) = action
getManageHooks (_, _, manageHooks) = manageHooks

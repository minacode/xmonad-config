module Main where

import Graphics.X11.ExtraTypes.XF86
  ( xF86XK_AudioLowerVolume 
  , xF86XK_AudioRaiseVolume 
  , xF86XK_AudioMute 
  , xF86XK_AudioPrev 
  , xF86XK_AudioNext 
  , xF86XK_AudioPlay 
  , xF86XK_MonBrightnessDown 
  , xF86XK_MonBrightnessUp 
  , xF86XK_Search
  )

import XMonad
  ( doF
  , (|||), (-->), (<||>), (=?)
  , appName, composeAll, launch, spawn, xmonad
  , mod4Mask 
  , MonadReader(ask)
  , Default(def)
  , XConfig
    ( modMask
    , workspaces, terminal, borderWidth, focusFollowsMouse
    , handleEventHook, layoutHook, manageHook, startupHook
    )
  , Full(Full)
  )
import XMonad.Actions.CycleWS
  ( nextWS, prevWS
  , shiftToNext, shiftToPrev
  , Direction1D( Next, Prev)
  )
import XMonad.Actions.SpawnOn
  ( manageSpawn
  , spawnOn
  )
import XMonad.Hooks.ScreenCorners
  ( addScreenCorners
  , screenCornerEventHook
  , screenCornerLayoutHook
  , ScreenCorner
    ( SCLowerLeft
    , SCLowerRight
    )
  )
import XMonad.Layout.NoBorders
  (smartBorders, noBorders)
import XMonad.Layout.MouseResizableTile
  (mouseResizableTile
  , DraggerType
    ( FixedDragger
    , gapWidth
    , draggerWidth
    )
  , MouseResizableTile 
    ( draggerType
    , nmaster
    , masterFrac
    , slaveFrac
    , fracIncrement
    )
  )
import XMonad.Util.EZConfig
  (additionalKeys, additionalKeysP, additionalMouseBindings)

-- Volume #############################################################################

myPASink       = "alsa_output.pci-0000_00_1b.0.analog-stereo"
myVolumeSetter = "pactl set-sink-volume"
myMuteSetter   = "pactl set-sink-mute"

incVolume  = spawn $ unwords [myVolumeSetter, myPASink, "+2%"]
decVolume  = spawn $ unwords [myVolumeSetter, myPASink, "-2%"]
muteVolume = spawn $ unwords [myMuteSetter, myPASink, "toggle"] 

-- Constants ########################################################################

backlightDecCmd = spawn "xbacklight -dec 3"
backlightIncCmd = spawn "xbacklight -inc 3"

mpPlayPause     = spawn "playerctl play-pause"
mpPrev          = spawn "playerctl previous"
mpNext          = spawn "playerctl next"

appLauncher     = "rofi -show drun"
myTerminal      = "alacritty"

-- main ###########################################################################

main = 
  xmonad $ def
    { modMask           = mod4Mask
    , workspaces        = map show [1..9]
    , terminal          = myTerminal
    , borderWidth       = 0
    , focusFollowsMouse = True
    , handleEventHook   = screenCornerEventHook
    , layoutHook        = myLayoutHook
    , manageHook        = myManageHook
    , startupHook       = myStartupHook
    }
    `additionalKeysP` myKeysP 
    `additionalKeys`  myKeys
 
-- Controls #######################################################################
 
myKeysP = 
  [ ("M1-<Tab>", nextWS)
  , ("M1-S-<Tab>", shiftToNext >> nextWS)
  , ("M-<Tab>", prevWS)
  , ("M-S-<Tab>", shiftToPrev >> prevWS)
  , ("M-g", spawn appLauncher) 
  , ("M-n", spawn "networkmanager_dmenu")
  , ("M-p", spawn "passmenu")
  ]

myKeys = 
  [ ((0, xF86XK_AudioLowerVolume) , decVolume)
  , ((0, xF86XK_AudioRaiseVolume) , incVolume)
  , ((0, xF86XK_AudioMute)        , muteVolume)
  , ((0, xF86XK_AudioPrev)        , mpPrev)
  , ((0, xF86XK_AudioNext)        , mpNext)
  , ((0, xF86XK_AudioPlay)        , mpPlayPause)
  , ((0, xF86XK_MonBrightnessDown), backlightDecCmd)
  , ((0, xF86XK_MonBrightnessUp)  , backlightIncCmd)
  , ((0, xF86XK_Search)           , spawn appLauncher)
  ]

-- Hooks #############################################################################

myLayoutHook =
  screenCornerLayoutHook $ 
  noBorders $
  mouseResizableTile 
    { draggerType = FixedDragger 
      { gapWidth = 10
      , draggerWidth = 10
      }
    , nmaster = 1
    , masterFrac = 1/2
    , slaveFrac = 1/2
    , fracIncrement = 3/100 
    } ||| 
  Full 

myManageHook = composeAll 
  [ manageSpawn
  , manageHook def
  ]

myStartupHook = do 
  addScreenCorners 
    [ (SCLowerRight, nextWS)
    , (SCLowerLeft,  prevWS)
    ]
  spawnOn "9" $ unwords [myTerminal, "-e", "htop"]
  spawn appLauncher


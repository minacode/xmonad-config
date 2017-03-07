{-#  LANGUAGE RecordWildCards #-}

import XMonad
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.DynamicLog
import XMonad.Config.Desktop
import XMonad.Util.EZConfig
import XMonad.Util.Run
import XMonad.Util.Loggers
import XMonad.Layout.NoBorders

data Lemonbar = Lemonbar {
  path            :: String,
  width           :: Int,
  height          :: Int,
  x               :: Int,
  y               :: Int,
  foreground      :: String,
  background      :: String,
  font            :: String,
  underlineWidth :: Int,
  underlineColor :: String
} 

instance Show Lemonbar where
  show Lemonbar {..} = unwords
    [ path
    , "-g", show width ++ "x" ++ show height ++ "+" ++ show x ++ "+" ++ show y
    , "-F", show foreground
    , "-B", show background
    , "-f-", show font
    , "-u", show underlineWidth
    , "-U", show underlineColor
    , "-p | bash"
    ]

main = do
  lemonbar <- spawnPipe myLemonbar
  xmonad $ defaultConfig 
    { modMask         = myModMask
    , terminal        = myTerminal
    , manageHook      = myManageHook
    , layoutHook      = myLayoutHook
    , handleEventHook = myHandleEventHook
    , startupHook     = startupHook desktopConfig
    , logHook         = myLogHook lemonbar
    } `additionalKeysP`  
    myKeysP


myTerminal = "gnome-terminal"

myLemonbar = show Lemonbar
  { path       = "/bin/lemonbar"
  , width      = 1920
  , height     = 20
  , x          = 0
  , y          = 0
  , foreground = "#99BF9C"
  , background = "#100B1C"
  , font       = "Liberation Mono:size=12"
  , underlineWidth = 0
  , underlineColor = "#000000"
  }

myModMask = mod4Mask

myManageHook = 
  manageDocks <+> 
  (isFullscreen --> doFullFloat) <+> 
  manageHook defaultConfig

myLayoutHook = avoidStruts $ smartBorders $ layoutHook defaultConfig

myHandleEventHook = do
  handleEventHook defaultConfig
  docksEventHook
  fullscreenEventHook

wrapIn s = wrap s s

myLogHook h = dynamicLogWithPP $ defaultPP
  { ppOutput  = hPutStrLn h
  , ppCurrent = swapColors
  , ppHiddenNoWindows = id
  , ppTitle   = const ""
  , ppLayout  = const ""
  , ppUrgent  = id
  , ppExtras  = myExtraLoggers
  , ppSep     = " "
  }

myExtraLoggers :: [Logger]
myExtraLoggers = 
  [ logOffset 25 $ logCmd "iwgetid -r"
  , logOffset 25 $ battery
  , logCenter $ date "%a, %d.%m.%y, %T" 
  ]

left   = wrapIn "%{l}"
center = wrapIn "%{c}"
right  = wrapIn "%{r}"
swapColors = wrapIn "%{R}"
withCommand c = wrap ("%{A:" ++ c ++ ":}") "%{A}"

offset :: Int -> String -> String
offset o = wrap ("%{O" ++ show o ++ "}") "%{O}"

logLeft   = onLogger left
logCenter = onLogger center
logRight  = onLogger right
logOffset o = onLogger $ offset o
logCommand c = onLogger $ withCommand c

myAudioVolume = logCmd "pactl ..."

myKeysP = 
  [ ("M-p", spawn "dmenu_run -fn 'Vera Sans Mono-11'")
  , ("M-c", spawn "chromium")
  , ("M-n", spawn "nautilus")
  , ("M-s", spawn $ "pactl set-sink-mute " ++ myAudioSink ++ " toggle")
  , ("M-a", spawn $ "pactl set-sink-volume " ++ myAudioSink ++ " " ++ myAudioDownRate)
  , ("M-d", spawn $ "pactl set-sink-volume " ++ myAudioSink ++ " " ++ myAudioUpRate)
  , ("M-w", spawn $ "xbacklight -dec " ++ myBacklightDec)
  , ("M-e", spawn $ "xbacklight -inc " ++ myBacklightInc)
  ]

myAudioSink = "alsa_output.pci-0000_00_1b.0.analog-stereo"
myAudioDownRate = "-2%"
myAudioUpRate = "+2%"

myBacklightDec = show 3
myBacklightInc = show 3

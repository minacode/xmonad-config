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
import XMonad.ManageHook
import XMonad.Actions.SpawnOn
import Graphics.X11.ExtraTypes.XF86

-- Lemonbar ######################################################################

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
    , "-p"
    ]

defaultLemonbar = show Lemonbar
  { path       = "/bin/lemonbar"
  , width      = 1720
  , height     = 20
  , x          = 0
  , y          = 0
  , foreground = "#99BF9C"
  , background = "#100B1C"
  , font       = "Liberation Mono:size=12"
  , underlineWidth = 0
  , underlineColor = "#000000"
  }

-- Constants #########################################################################

myModMask = mod4Mask

myTerminal = "gnome-terminal"
mySystray = "trayer --edge top --align right --widthtype pixel --width 200 --height 20 --SetDockType true --transparent true --alpha 0 --tint 0x100B1C"
myFileManager = "nautilus"
myBrowser = "chromium"
myEmailClient = "thunderbird"
myNetworkManager = "nm-applet"
myCloud = "owncloud"
myBackground = "feh --bg-scale /home/max/Pictures/Wallpapers/" ++ myBackgroundImage
myRedshift = "redshift"
myAudioControl = "volctl"

myBackgroundImage = "1.jpg"
myAudioSink = "alsa_output.pci-0000_00_1b.0.analog-stereo"
myAudioDownRate = "-2%"
myAudioUpRate = "+2%"

myBacklightDec = show 3
myBacklightInc = show 3

-- main ###########################################################################

main = do
  lemonbar <- spawnPipe defaultLemonbar
  xmonad $ defaultConfig 
    { modMask         = myModMask
    , terminal        = myTerminal
    , manageHook      = manageSpawn <+> myManageHook
    , layoutHook      = myLayoutHook
    , handleEventHook = myHandleEventHook
    , startupHook     = myStartupHook
    , logHook         = myLogHook lemonbar
    } `additionalKeysP`  myKeysP `additionalKeys` myKeys
   

myKeysP = 
  [ ("M-p", spawn "dmenu_run -fn 'Vera Sans Mono-11'")
  , ("M-c", spawn myBrowser)
  , ("M-n", spawn myFileManager)
  ]

myKeys = 
  [ ((0, xF86XK_AudioLowerVolume), spawn $ unwords ["pactl", "set-sink-volume", myAudioSink, myAudioDownRate])
  , ((0, xF86XK_AudioRaiseVolume), spawn $ unwords ["pactl", "set-sink-volume", myAudioSink, myAudioUpRate])
  , ((0, xF86XK_AudioMute), spawn $ unwords ["pactl", "set-sink-mute", myAudioSink, "toggle"])
  , ((0, xF86XK_MonBrightnessDown), spawn $ unwords ["xbacklight", "-dec", myBacklightDec])
  , ((0, xF86XK_MonBrightnessUp), spawn $ unwords ["xbacklight", "-inc", myBacklightInc])
  ]

-- Hooks #############################################################################

myManageHook = 
  composeAll
  [ manageDocks
  , (isFullscreen --> doFullFloat)
  , resource =? "owncloud" --> doShift "9:owncloud"
  , manageHook defaultConfig
  ]

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

myStartupHook = do
  startupHook desktopConfig
  spawn mySystray
  spawn myNetworkManager
  spawn myBackground
  spawn myRedshift
  spawn myAudioControl
  spawn myCloud
  spawnOn "9" myEmailClient

-- Logger ###########################################################################

myExtraLoggers :: [Logger]
myExtraLoggers = 
  [ logOffset 25 $ logCmd "iwgetid -r"
  , logOffset 25 $ battery
  , logCenter . (logOffset 200) $ date "%a, %d.%m.%y, %T" 
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

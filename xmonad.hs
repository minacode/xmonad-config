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
  underlineWidth  :: Int,
  underlineColor  :: String
} 

instance Show Lemonbar where
  show Lemonbar {..} = unwords
    [ path
    , "-g",  show width ++ "x" ++ show height ++ "+" ++ show x ++ "+" ++ show y
    , "-F",  show foreground
    , "-B",  show background
    , "-f-", show font
    , "-u",  show underlineWidth
    , "-U",  show underlineColor
    , "-p"
    ]

defaultLemonbar = Lemonbar
  { path           = "/bin/lemonbar"
  , width          = 1720
  , height         = 20
  , x              = 0
  , y              = 0
  , foreground     = "#99BF9C"
  , background     = "#100B1C"
  , font           = "Liberation Mono:size=12"
  , underlineWidth = 0
  , underlineColor = "#000000"
  }

-- Constants #########################################################################

myModMask = mod4Mask

myForegroundColor = "99BF9C"
myBackgroundColor = "100B1C"

myTerminal        = "gnome-terminal"
mySystray         = unwords 
  [ "trayer"
  , "--edge", "top"
  , "--align", "right"
  , "--widthtype", "pixel"
  , "--width", show 200
  , "--height", show 20
  , "--SetDockType", "true"
  , "--transparent", "true"
  , "--alpha", show 0
  , "--tint", "0x" ++ myBackgroundColor
  ]
myLauncher        = "dmenu_run -fn 'Vera Sans Mono-11'-b -l 5"
myFileManager     = "nautilus"
myBrowser         = "chromium"
myEmailClient     = "thunderbird"
myNetworkManager  = "nm-applet"
myCloud           = "owncloud"
myBackground      = "feh --bg-scale /home/max/Pictures/Wallpapers/" ++ myBackgroundImage
myRedshift        = "redshift"
myAudioControl    = "volctl"
myScreenLock      = "sflock"

myBackgroundImage = "1.jpg"

myAudioSink       = "alsa_output.pci-0000_00_1b.0.analog-stereo"
myAudioDownRate   = "-2%"
myAudioUpRate     = "+2%"
myAudioChangeCmd  = unwords ["pactl", "set-sink-volume"]
myAudioDownCmd    = unwords [myAudioChangeCmd, myAudioSink, myAudioDownRate]
myAudioUpCmd      = unwords [myAudioChangeCmd, myAudioSink, myAudioUpRate]
myAudioMuteCmd    = unwords ["pactl", "set-sink-mute", myAudioSink, "toggle"]

myBacklightDec    = show 3
myBacklightInc    = show 3
myBacklightDecCmd = unwords ["xbacklight", "-dec", myBacklightDec]
myBacklightIncCmd = unwords ["xbacklight", "-inc", myBacklightInc]

-- main ###########################################################################

main = do
  lemonbar <- spawnPipe . show $ defaultLemonbar
    { foreground = '#' : myForegroundColor
    , background = '#' : myBackgroundColor
    }
  xmonad $ defaultConfig 
    { modMask         = myModMask
    , terminal        = myTerminal
    , manageHook      = manageSpawn <+> myManageHook
    , layoutHook      = myLayoutHook
    , handleEventHook = myHandleEventHook
    , startupHook     = myStartupHook
    , logHook         = myLogHook lemonbar
    } `additionalKeysP` myKeysP `additionalKeys` myKeys
   

myKeysP = 
  [ ("M-p", spawn myLauncher)
  , ("M-c", spawn myBrowser)
  , ("M-n", spawn myFileManager)
  , ("M-l", spawn myScreenLock)
  ]

myKeys = 
  [ ((0, xF86XK_AudioLowerVolume) , spawn myAudioDownCmd) 
  , ((0, xF86XK_AudioRaiseVolume) , spawn myAudioUpCmd) 
  , ((0, xF86XK_AudioMute)        , spawn myAudioMuteCmd)
  , ((0, xF86XK_MonBrightnessDown), spawn myBacklightDecCmd)
  , ((0, xF86XK_MonBrightnessUp)  , spawn myBacklightIncCmd)
  ]

-- Hooks #############################################################################

myManageHook = 
  composeAll
    [ manageDocks
    , (isFullscreen --> doFullFloat)
    , manageHook defaultConfig
    ]

myLayoutHook = avoidStruts $ smartBorders $ layoutHook defaultConfig

myHandleEventHook = do
  handleEventHook defaultConfig
  docksEventHook
  fullscreenEventHook

myLogHook h = dynamicLogWithPP $ defaultPP
  { ppOutput          = hPutStrLn h
  , ppCurrent         = swapColors
  , ppHiddenNoWindows = id
  , ppTitle           = const ""
  , ppLayout          = const ""
  , ppUrgent          = id
  , ppExtras          = myExtraLoggers
  , ppSep             = " "
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

wrapIn s      = wrap s s
left          = wrapIn "%{l}"
center        = wrapIn "%{c}"
right         = wrapIn "%{r}"
swapColors    = wrapIn "%{R}"
withCommand c = wrap ("%{A:" ++ c ++ ":}") "%{A}"

offset :: Int -> String -> String
offset o = wrap ("%{O" ++ show o ++ "}") "%{O}"

logLeft      = onLogger left
logCenter    = onLogger center
logRight     = onLogger right
logOffset o  = onLogger $ offset o
logCommand c = onLogger $ withCommand c

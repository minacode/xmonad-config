{-#  LANGUAGE RecordWildCards #-}

import XMonad hiding (currentTime)
import XMonad.StackSet hiding (workspaces)
import XMonad.Hooks.ManageDocks 
  (docks, avoidStruts)
import XMonad.Hooks.ManageHelpers
  (isFullscreen, doFullFloat)
import XMonad.Hooks.EwmhDesktops
  (ewmh, fullscreenEventHook)
import XMonad.Hooks.DynamicLog
  ( ppOutput, ppCurrent, ppHiddenNoWindows, ppTitle
  , ppLayout, ppUrgent, ppExtras, ppSep
  , dynamicLogWithPP, wrap
  ) 
import XMonad.Hooks.Place
  (placeHook, inBounds, underMouse)
import XMonad.Hooks.WallpaperSetter
  (wallpaperSetter, Wallpaper(..), WallpaperList(..), WallpaperConf(..))
import XMonad.Config.Desktop
  (desktopConfig)
import XMonad.Util.EZConfig
  (additionalKeys, additionalKeysP)
import XMonad.Util.Run
  (spawnPipe)
import XMonad.Util.Loggers
  (Logger, battery, date, onLogger)
import XMonad.Layout.NoBorders 
  (smartBorders)
import XMonad.Layout.AvoidFloats
  (avoidFloats)
import XMonad.Actions.CycleWS
  (nextWS, prevWS, shiftToNext, shiftToPrev, shiftTo, WSType(..), Direction1D(..))
import Graphics.X11.ExtraTypes.XF86
import Data.Time.Clock
  (getCurrentTime)
import Data.Time.LocalTime
  (getCurrentTimeZone, TimeOfDay(..), utcToLocalTime, localTimeOfDay)
import System.IO
  (hPrint, hPutStrLn, hClose, openFile, IOMode(..))

-- Wallpaper #########################################################################

myWallpaperPath  = "/home/max/Pictures/Wallpapers/"
myDayWallpaper   = "1.jpg"
myNightWallpaper = "5.jpg"

myWallpaper = do
  time <- currentTime
  return $ getWallpaperImage time

getWallpaperImage time 
  | isNight time = WallpaperFix myNightWallpaper
  | otherwise    = WallpaperFix myDayWallpaper

setWallpaper = do
  wp <- io myWallpaper
  let wplist = WallpaperList $ zip myWorkspaces $ repeat wp
      conf   = WallpaperConf myWallpaperPath wplist
  wallpaperSetter conf

-- ALSA #############################################################################

myVolumeUp     = 2
myVolumeDown   = myVolumeUp

incVolume  = spawn $ unwords [myAudioControl, '+' : show myVolumeUp]
decVolume  = spawn $ unwords [myAudioControl, '-' : show myVolumeDown]
muteVolume = spawn $ unwords [myAudioControl, "mute"]  

-- Constants ########################################################################

myModMask = mod4Mask

myForegroundColor = "99BF9C"
myBackgroundColor = "100B1C"

myWorkspaces      = map show [1..9]
myTerminal        = "gnome-terminal"
mySystray         = unwords 
  [ "trayer"
  , "--edge", "top"
  , "--align", "right"
  , "--widthtype", "pixel"
  , "--width", show 200
  , "--height", show 22
  , "--SetDockType", "true"
  , "--transparent", "true"
  , "--alpha", show 0
  , "--tint", "0x" ++ myBackgroundColor
  ]
myLauncher        = "xfce4-appfinder"
myFileManager     = "nautilus"
myBrowser         = "chromium"
myEmailClient     = "thunderbird"
myNetworkManager  = "nm-applet"
myCloud           = "nextcloud"
myRedshift        = "redshift-gtk"
myAudioControl    = "alsa-tray"
myScreenLock      = "sflock"
myBar             = show $ def 
  { foreground = '#' : myForegroundColor
  , background = '#' : myBackgroundColor
  }

myBacklightDec    = show 3
myBacklightInc    = show 3
myBacklightDecCmd = unwords ["xbacklight -dec", myBacklightDec]
myBacklightIncCmd = unwords ["xbacklight -inc", myBacklightInc]

myMediaPlayer     = "dbus-send --session --print-reply --dest=org.mpris.MediaPlayer2.vlc /org/mpris/MediaPlayer2"
myMPCmd c       = unwords [myMediaPlayer, "org.mpris.MediaPlayer2.Player." ++ c]
myMPPlay        = myMPCmd "Play"
myMPPause       = myMPCmd "Pause"
myMPPlayPause   = myMPCmd "PlayPause"
myMPPrev        = myMPCmd "Previous"
myMPNext        = myMPCmd "Next"

mySunrise = TimeOfDay 6 0 0
mySunset  = TimeOfDay 21 0 0

-- main ###########################################################################

main = do
  xmonad $ docks $ ewmh def 
    { modMask         = myModMask
    , workspaces      = myWorkspaces
    , terminal        = myTerminal
    , manageHook      = myManageHook
    , layoutHook      = myLayoutHook
    , handleEventHook = myHandleEventHook
    , startupHook     = myStartupHook
    , logHook         = myLogHook
    }
    `additionalKeysP` myKeysP 
    `additionalKeys`  myKeys
  
myKeysP = 
  [ ("M-p", spawn myLauncher)
  , ("M-c", spawn myBrowser)
  , ("M-n", spawn myFileManager)
  , ("M1-<Tab>", nextWS)
  , ("M1-S-<Tab>", prevWS)
  , ("M-<Tab>", shiftToNext >> nextWS)
  , ("M-S-<Tab>", shiftToPrev >> prevWS)
  , ("M-w", shiftTo Next EmptyWS) 
  ] 

myKeys = 
  [ ((0, xF86XK_AudioLowerVolume) , decVolume)
  , ((0, xF86XK_AudioRaiseVolume) , incVolume)
  , ((0, xF86XK_AudioMute)        , muteVolume)
  , ((0, xF86XK_MonBrightnessDown), spawn myBacklightDecCmd)
  , ((0, xF86XK_MonBrightnessUp)  , spawn myBacklightIncCmd)
  , ((0, xF86XK_AudioPrev)        , spawn myMPPrev)
  , ((0, xF86XK_AudioNext)        , spawn myMPNext)
  , ((0, xF86XK_AudioPlay)        , spawn myMPPlayPause)
  ]

-- Hooks #############################################################################

myManageHook = 
  composeAll
    [ appName =? myAudioControl --> placeHook myPlacement
    , isFullscreen --> doFullFloat
    ,      appName =? myCloud 
      <||> appName =? "owncloud"
      <||> appName =? myBrowser
      <||> appName =? myFileManager
      -->  unfloat
    , appName =? myAudioControl --> doFloat
    , manageHook def
    ]

-- does not work as expected
myPlacement = inBounds $ underMouse (0,0)

myLayoutHook = 
  avoidStruts $
  avoidFloats $ 
  smartBorders $ 
  layoutHook def

myHandleEventHook = do
  handleEventHook def
  fullscreenEventHook

myLogHook = setWallpaper

myStartupHook = do
  startupHook desktopConfig
  spawn myNetworkManager
  spawn myRedshift
  --spawn myAudioControl
  spawn "tint2"

-- Helper ###########################################################################

unfloat = ask >>= doF . sink

currentTime = do
  utcNow <- getCurrentTime
  timezone <- getCurrentTimeZone
  return . localTimeOfDay $ utcToLocalTime timezone utcNow

between :: Ord a => a -> (a, a) -> Bool
between a (b, c) = (b <= a) &| (a <= c)
  where 
    (&|) :: Bool -> Bool -> Bool
    (&|) 
      | b <= c = (&&)
      | otherwise = (||) 

isNight :: TimeOfDay -> Bool
isNight time = time `between` (mySunset, mySunrise) 

myLog :: Show a => a -> IO ()
myLog l = do
  f <- openFile "/home/max/xmonadlog" AppendMode
  hPrint f l
  hClose f

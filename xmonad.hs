{-#  LANGUAGE RecordWildCards #-}

import qualified Data.Map as M
import qualified Data.Text as T

import XMonad hiding (currentTime)
import XMonad.StackSet hiding (workspaces, float)
import XMonad.Hooks.ManageDocks 
  (docks, avoidStruts)
import XMonad.Hooks.ManageHelpers
  (isFullscreen, doFullFloat)
import XMonad.Hooks.EwmhDesktops
  (ewmh, fullscreenEventHook)
import XMonad.Hooks.DynamicLog
  ( ppOutput, ppCurrent, ppHiddenNoWindows, ppTitle
  , ppLayout, ppUrgent, ppExtras, ppSep
  , dynamicLogWithPP, wrap) 
import XMonad.Hooks.Place
  (placeHook, inBounds, underMouse)
import XMonad.Hooks.WallpaperSetter
  (wallpaperSetter, Wallpaper(..), WallpaperList(..), WallpaperConf(..))
import XMonad.Config.Desktop
  (desktopConfig)
import XMonad.Util.EZConfig
  (additionalKeys, additionalKeysP, additionalMouseBindings)
import XMonad.Util.Run 
  (spawnPipe)
import XMonad.Layout.NoBorders 
  (smartBorders)
import XMonad.Layout.AvoidFloats
  (avoidFloats)
import XMonad.Actions.CycleWS
  (nextWS, prevWS, shiftToNext, shiftToPrev, shiftTo, WSType(..), Direction1D(..))
import XMonad.Actions.MouseGestures
import XMonad.Actions.Plane
import XMonad.Actions.WindowBringer
import XMonad.Actions.WindowGo
import XMonad.Actions.WindowMenu
import XMonad.Hooks.ScreenCorners
import XMonad.Layout.MouseResizableTile
import XMonad.Util.NamedWindows (getName)
import Graphics.X11.ExtraTypes.XF86
import Data.Time.Clock
import Data.Time.LocalTime
import Data.Time.Horizon
import Data.Time.Calendar
import System.IO 
  (hPrint, hPutStrLn, hClose, openFile, IOMode(..))

-- Wallpaper #########################################################################

myWallpaperPath  = "/home/max/Pictures/Wallpapers/"
myDayWallpaper   = "1.jpg"
myNightWallpaper = "5.jpg"

myWallpaper = do
  time <- getCurrentTime
  let day = utctDay time
  return $ getWallpaperImage time day

getWallpaperImage :: UTCTime -> Day -> Wallpaper
getWallpaperImage time day 
  | isNight time day myLongitudeWest myLatitudeNorth = WallpaperFix myNightWallpaper
  | otherwise                                        = WallpaperFix myDayWallpaper

setWallpaper = do
  wp <- io myWallpaper
  let wplist = WallpaperList $ zip myWorkspaces $ repeat wp
      conf   = WallpaperConf myWallpaperPath wplist
  wallpaperSetter conf

-- Volume #############################################################################

myVolumeChange = "2%"
myVolumeUp     = '+' : myVolumeChange
myVolumeDown   = '-' : myVolumeChange

myPASink = "alsa_output.pci-0000_00_1b.0.analog-stereo"
myVolumeSetter = unwords ["pactl", "set-sink-volume"]
myMuteSetter = unwords ["pactl", "set-sink-mute"]

incVolume  = spawn $ unwords [myVolumeSetter, myPASink, myVolumeUp]
decVolume  = spawn $ unwords [myVolumeSetter, myPASink, myVolumeDown]
muteVolume = spawn $ unwords [myMuteSetter, myPASink, "toggle"]  

-- Constants ########################################################################

myModMask = mod4Mask

myForegroundColor = "99BF9C"
myBackgroundColor = "100B1C"

myWorkspaces      = map show [1..9]
myTerminal        = "gnome-terminal"
myLauncher        = "xfce4-appfinder"
myBar             = "tint2"
myFileManager     = "nautilus"
myBrowser         = "chromium"
myEmailClient     = "thunderbird"
myNetworkManager  = "nm-applet"
myCloud           = "nextcloud"
myRedshift        = "redshift-gtk"
myAudioControl    = "volctl"
myScreenLock      = "sflock"
myScreenshotCmd   = "import -window root screenshot.jpg"

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

myWindowBringerConfig :: WindowBringerConfig
myWindowBringerConfig = def
  { menuArgs = ["-i", "-l", "20", "-b"]
  , windowTitler = myWindowBringerTitler
  }
myWindowBringerColumnSize = 50

--mySunrise = TimeOfDay 6 0 0
--mySunset  = TimeOfDay 21 0 0

myLongitudeWest = 13.737262
myLatitudeNorth = 51.050407

-- main ###########################################################################

main =  
  xmonad $ 
  docks $ 
  ewmh def
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
    `additionalMouseBindings` myMouseBindings
 
-- Controls #######################################################################
 
myKeysP = 
  [ ("M-p", spawn myLauncher)
  , ("M-c", spawn myBrowser)
  , ("M-n", spawn myFileManager)
  , ("M-m", spawn myArmyListTool)
  , ("M-i", spawn myScreenshotCmd)
  , ("M-o", gotoMenuConfig myWindowBringerConfig)
  , ("M-S-o", bringMenuConfig myWindowBringerConfig)
  , ("M1-<Tab>", nextWS)
  , ("M1-S-<Tab>", shiftToNext >> nextWS)
  , ("M-<Tab>", prevWS)
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
  ++ M.toList (planeKeys myModMask (Lines 1) Linear)

myMouseBindings =
  [ ((myModMask, button3), mouseGesture myGestures)
  , ((0, button2), const windowMenu)
  ]

myGestures = M.fromList 
  [ ([U], float)
  ]

myScreenCorners = 
  [ (SCLowerRight, nextWS)
  , (SCLowerLeft,  prevWS)
  --, (SCUpperRight, runOrRaiseMaster myLauncher (appName =? myLauncher))
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
  screenCornerLayoutHook $ 
  avoidStruts $
  avoidFloats $ 
  smartBorders $
  mouseResizableTile 
    { draggerType = BordersDragger
    , nmaster = nmaster
    , masterFrac = ratio
    , slaveFrac = ratio
    , fracIncrement = ratioInc 
    }
  ||| tiled ||| Mirror tiled ||| Full
  where
    tiled    = Tall nmaster ratioInc ratio
    nmaster  = 1
    ratioInc = 3/100 
    ratio    = 1/2

myHandleEventHook =
  composeAll
    [ screenCornerEventHook
    , fullscreenEventHook
    ]

myLogHook = setWallpaper

myStartupHook = do
  startupHook desktopConfig
  addScreenCorners myScreenCorners
  spawn myNetworkManager
  spawn myRedshift
  spawn myAudioControl
  spawn myBar
  spawn myCloud

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

isNight :: UTCTime -> Day -> LongitudeWest -> LatitudeNorth -> Bool
isNight time day long lat = time `between`
  ( sunset day long lat
  , sunrise day long lat
  ) 

myWindowBringerTitler :: WindowSpace -> Window -> X String
myWindowBringerTitler ws w = do
  description <- show <$> getName w
  return $ descriptionToLine description

descriptionToLine :: String -> String
descriptionToLine d = 
  let (name, title) = splitInformation d
      name' = fillWithSpaces myWindowBringerColumnSize name
   in name' ++ title 

splitInformation :: String -> (String, String)
splitInformation d = mapTuple (T.unpack . T.reverse . T.strip . T.pack) $ span (/= '-') (reverse d)
    
fillWithSpaces len str
  | ls < len = str ++ replicate (len -  ls) ' '
  | otherwise = str 
  where  
    ls = length str 
  
mapTuple :: (a -> b) -> (a, a) -> (b, b)
mapTuple f (a, b) = (f a, f b)

myLog :: Show a => a -> IO ()
myLog l = do
  f <- openFile "/home/max/xmonadlog" AppendMode
  hPrint f l
  hClose f

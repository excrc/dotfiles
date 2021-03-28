import           Control.Monad
import qualified Data.Map                     as M
import           Data.Maybe
import           Data.Monoid
import           System.Exit
import           System.IO
import           XMonad
import           XMonad.Hooks.DynamicLog
import           XMonad.Hooks.EwmhDesktops
import           XMonad.Hooks.ManageDocks
import           XMonad.Hooks.ManageHelpers
import           XMonad.Hooks.SetWMName
import           XMonad.Layout.LayoutModifier
import           XMonad.Layout.NoBorders
import qualified XMonad.StackSet              as W
import           XMonad.Util.EZConfig
import           XMonad.Util.Run
import           XMonad.Util.SpawnOnce

myModMask :: KeyMask
myModMask = mod4Mask

myTerminal :: String
myTerminal = "alacritty"

myBrowser :: String
myBrowser = "firefox"

myEditor :: String
myEditor = myTerminal ++ " -e vim "

myBorderWidth :: Dimension
myBorderWidth = 2

normColor, focusColor :: String
normColor = "#282c34"
focusColor = "#bbc5ff"

windowCount :: X (Maybe String)
windowCount = gets $
  Just . show . length . W.integrate' . W.stack . W.workspace . W.current . windowset

myStartupHook :: X ()
myStartupHook = do
  --spawnOnce $ concat
   -- [ "trayer"
   -- , " --edge top"
   -- , " --align right"
   -- , " --widthtype request"
   -- , " --expand true"
   -- , " --SetDockType true"
   -- , " --SetPartialStrut false"
   -- , " --transparent true"
   -- , " --alpha 0"
   -- , " --tint 0x1d1f28"
   -- , " --expand true"
   -- , " --heighttype pixel"
   -- , " --height 16"
   -- , " --monitor 0"
   -- , " --padding 1"
   -- , " --distance 2"
   -- , " --distancefrom top &"
   -- ]
  setWMName "LG3D"
  addEWMHFullscreen

myLayoutHook ::
  ModifiedLayout SmartBorder
    (ModifiedLayout AvoidStruts
      (Choose Tall
        (Choose (Mirror Tall)
          Full
        )
      )
    )
  Window
myLayoutHook = smartBorders . avoidStruts $ layoutHook def

myWorkspaces :: [String]
myWorkspaces = map show [1 .. 9]

myManageHook :: XMonad.Query (Data.Monoid.Endo WindowSet)
myManageHook = composeAll
  [ className =? "Gimp" --> doFloat
  , (className =? "firefox" <&&> resource =? "Dialog") --> doFloat
  ]

myKeys :: [(String,X ())]
myKeys =
  [ ("M-S-r", spawn "xmonad --recompile && xmonad --restart")
  , ("M-S-q", io exitSuccess)
  , ("M-<Return>", spawn myTerminal)
  ]

myLogHook :: Handle -> X ()
myLogHook = dynamicLogWithPP . myXmobarPP

myXmobarPP :: Handle -> PP
myXmobarPP h = xmobarPP
  { ppOutput = hPutStrLn h
  , ppCurrent = xmobarColor "#ffc552" "" . wrap "[" "]"
  , ppHidden = xmobarColor "#abb2bf" ""
  , ppUrgent = xmobarColor orange "" .  wrap "!" "!"
  , ppExtras = [windowCount]
  , ppSep = " | "
  , ppOrder = \(ws:_) -> [ws]
  }

green, blue, orange, base01 :: String
green   = "#859900"
blue    = "#268bd2"
orange  = "#cb4b16"
base01  = "#586e75"
base1   = "#93a1a1"
base02  = "#002b36"


addNETSupported :: Atom -> X ()
addNETSupported x = withDisplay $ \dpy -> do
  r <- asks theRoot
  a_NET_SUPPORTED <- getAtom "_NET_SUPPORTED"
  a <- getAtom "ATOM"
  liftIO $ do
    sup <- (join . maybeToList) <$>
      getWindowProperty32 dpy a_NET_SUPPORTED r
    when (fromIntegral x `notElem` sup) $
      changeProperty32 dpy r a_NET_SUPPORTED a propModeAppend [fromIntegral x]

addEWMHFullscreen :: X ()
addEWMHFullscreen = do
  wms <- getAtom "_NET_WM_STATE"
  wfs <- getAtom "_NET_WM_STATE_FULLSCREEN"
  mapM_ addNETSupported [wms, wfs]

myHandleEventHook :: Event -> X All
myHandleEventHook = handleEventHook def <+> fullscreenEventHook

main :: IO ()
main = do
  h <- spawnPipe "xmobar /home/ecquo/.config/xmobar/xmobar.hs"
  xmonad . ewmh $ docks $ def
    { manageHook = myManageHook
    , handleEventHook = myHandleEventHook
    , modMask = myModMask
    , terminal = myTerminal
    , startupHook = myStartupHook
    , layoutHook = myLayoutHook
    , workspaces = myWorkspaces
    , borderWidth = myBorderWidth
    , normalBorderColor = normColor
    , focusedBorderColor = focusColor
    , logHook = myLogHook h
    } `additionalKeysP` myKeys

--MY PERSONAL CONFIG--
import XMonad
import XMonad.Prompt
import XMonad.Prompt.RunOrRaise (runOrRaisePrompt)
import XMonad.Prompt.AppendFile (appendFilePrompt)
import XMonad.Operations
import System.IO
import System.Exit
import XMonad.Util.Run
import XMonad.Actions.CycleWS
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.SetWMName
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.UrgencyHook
import XMonad.Hooks.FadeInactive
import XMonad.Hooks.EwmhDesktops
import XMonad.Layout.NoBorders (smartBorders, noBorders)
import XMonad.Layout.PerWorkspace (onWorkspace, onWorkspaces)
import XMonad.Layout.Reflect (reflectHoriz)
import XMonad.Layout.IM
import XMonad.Layout.SimpleFloat
import XMonad.Layout.Spacing
import XMonad.Layout.ResizableTile
import XMonad.Layout.LayoutHints
import XMonad.Layout.LayoutModifier
import XMonad.Layout.Grid
import Data.Ratio ((%))
import qualified XMonad.StackSet as W
import qualified Data.Map as M

myTerminal      = "urxvt"
modMask' :: KeyMask
modMask' = mod4Mask
myWorkspaces    = ["Ξ" , "Φ", "~"] ++ ["α", "β"]
myXmonadBar = "dzen2 -x '0' -y '0' -h '24' -w '698' -ta 'l' -fn '-*-Liberation mono-*-r-normal-*-*-110-*-*-*-*-iso8859-*' -fg '#FFFFFF' -bg '#1B1D1E'"
myStatusBar = "conky -c /home/x4w3/.xmonad/.conky_dzen | dzen2 -x '650' -y '0' -w '452' -h '24' -ta 'r' -fn '-*-inconsolata-*-r-normal-*-*-120-*-*-*-*-iso8859-*' -bg '#1B1D1E' -fg '#FFFFFF'"
myBitmapDir = "/home/x4w3/.xmonad/dzen2"

main = do
    dzenLeftBar <- spawnPipe myXmonadBar
    dzenRightBar <- spawnPipe myStatusBar
    xmonad $ withUrgencyHookC dzenUrgencyHook { args = ["-bg", "red", "fg", "black", "-xs", "1", "-y", "25"] } urgencyConfig { remindWhen = Every 15 } $ defaultConfig
      { terminal            = myTerminal
      , workspaces          = myWorkspaces
      , keys                = keys'
      , modMask             = modMask'
      , layoutHook          = layoutHook'
      , manageHook          = manageDocks <+> myManageHook <+> manageHook defaultConfig
      , logHook             = myLogHook dzenLeftBar >> fadeInactiveLogHook 0xdddddddd
      , normalBorderColor   = colorNormalBorder
      , focusedBorderColor  = colorFocusedBorder
      , borderWidth         = 1
      , startupHook         = setWMName "LG3D"
}

myManageHook :: ManageHook
myManageHook = composeAll
                [ className =? "urxvt"     --> doShift "Ξ"
                , className =? "Thunar"           --> doShift "Ξ"
                , className =? "Chromium"       --> doShift "Φ"
                , className =? "subl"        --> doShift "Φ"
                , className =? "weechat"         --> doShift "~"
                , className =? "trayer"         --> doIgnore
                , isFullscreen                  --> (doF W.focusDown <+> doFullFloat)
                , manageDocks]

myDoFullFloat :: ManageHook
myDoFullFloat = doF W.focusDown <+> doFullFloat

layoutHook'  =  onWorkspaces ["1"] customLayout $ 
                customLayout2

myLogHook :: Handle -> X ()
myLogHook h = dynamicLogWithPP $ defaultPP
    {
        ppCurrent           =   dzenColor "#ebac54" "#1B1D1E" . pad
      , ppVisible           =   dzenColor "white" "#1B1D1E" . pad
      , ppHidden            =   dzenColor "white" "#1B1D1E" . pad
      , ppHiddenNoWindows   =   dzenColor "#7b7b7b" "#1B1D1E" . pad
      , ppUrgent            =   dzenColor "black" "red" . pad
      , ppWsSep             =   ""
      , ppSep               =   " "
      , ppLayout            =   dzenColor "#ebac54" "#1B1D1E" .
                                (\x -> case x of
                                    "ResizableTall"             ->      "^i(" ++ myBitmapDir ++ "/tall.xbm)"
                                    "Mirror ResizableTall"      ->      "^i(" ++ myBitmapDir ++ "/mtall.xbm)"
                                    "Full"                      ->      "^i(" ++ myBitmapDir ++ "/full.xbm)"
                                    "Simple Float"              ->      "~"
                                    _                           ->      x
                                )
      , ppTitle             =   (" " ++) . dzenColor "white" "#1B1D1E" . dzenEscape
      , ppOutput            =   hPutStrLn h
    }

customLayout = avoidStruts $ tiled ||| Mirror tiled ||| Full ||| simpleFloat
  where
    tiled   = ResizableTall 1 (2/100) (1/2) []

customLayout2 = avoidStruts $ Full ||| tiled ||| Mirror tiled ||| simpleFloat
  where
    tiled   = ResizableTall 1 (2/100) (1/2) []


colorOrange         = "#FD971F"
colorDarkGray       = "#1B1D1E"
colorPink           = "#F92672"
colorGreen          = "#FD971F"
colorBlue           = "#66D9EF"
colorYellow         = "#E6DB74"
colorWhite          = "#CCCCC6"
colorNormalBorder   = "#CCCCC6"
colorFocusedBorder  = "#fd971f"
barFont  = "Liberation mono:size=14"
barXFont = "Liberation mono:size=14"
xftFont = "xft:Liberation mono-12"

mXPConfig :: XPConfig
mXPConfig =
    defaultXPConfig { font                  = barFont
 
                    , bgColor               = colorDarkGray
                    , fgColor               = colorGreen
                    , bgHLight              = colorGreen
                    , fgHLight              = colorDarkGray
                    , promptBorderWidth     = 0
                    , height                = 16
                    , historyFilter         = deleteConsecutive
                    }
 
largeXPConfig :: XPConfig
largeXPConfig = mXPConfig
                { font = xftFont
                , height = 16
                }

keys' conf@(XConfig {XMonad.modMask = modMask}) = M.fromList $
    [ ((modMask,                    xK_p        ), runOrRaisePrompt largeXPConfig)
    , ((modMask .|. shiftMask,      xK_Return   ), spawn $ XMonad.terminal conf)
    , ((modMask .|. shiftMask,      xK_c        ), kill)
    , ((modMask .|. shiftMask,      xK_l        ), spawn "xscreesaver-command -lock")
    , ((0,                          xK_Print    ), spawn "/home/x4w3/imgur-screenshot/imgur-screenshot.sh")
    , ((modMask,		    xK_c        ), spawn "chromium")
    , ((modMask, 		    xK_s	), spawn "/usr/bin/subl")
    , ((modMask,                    xK_e        ), spawn "/usr/bin/thunar")
    , ((0,                          0x1008ff12  ), spawn "amixer -q sset Headphone toggle")
    , ((0,                          0x1008ff11  ), spawn "amixer -q sset Headphone 5%-")   
    , ((0,                          0x1008ff13  ), spawn "amixer -q sset Headphone 5%+")   
    , ((modMask,                    xK_space    ), sendMessage NextLayout)
    , ((modMask .|. shiftMask,      xK_space    ), setLayout $ XMonad.layoutHook conf)
    , ((modMask,                    xK_b        ), sendMessage ToggleStruts)
    , ((modMask,                    xK_n        ), refresh)
    , ((modMask,                    xK_Tab      ), windows W.focusDown)
    , ((modMask,                    xK_j        ), windows W.focusDown)
    , ((modMask,                    xK_k        ), windows W.focusUp  )
    , ((modMask .|. shiftMask,      xK_j        ), windows W.swapDown)
    , ((modMask .|. shiftMask,      xK_k        ), windows W.swapUp)
    , ((modMask,                    xK_Return   ), windows W.swapMaster)
    , ((modMask,                    xK_t        ), withFocused $ windows . W.sink)
    , ((modMask,                    xK_h        ), sendMessage Shrink)
    , ((modMask,                    xK_l        ), sendMessage Expand)
    , ((modMask,                    xK_comma    ), sendMessage (IncMasterN 1))
    , ((modMask,                    xK_period   ), sendMessage (IncMasterN (-1)))
    , ((modMask .|. controlMask,    xK_Right    ), nextWS)
    , ((modMask .|. shiftMask,      xK_Right    ), shiftToNext)
    , ((modMask .|. controlMask,    xK_Left     ), prevWS)
    , ((modMask .|. shiftMask,      xK_Left     ), shiftToPrev)
    , ((modMask .|. shiftMask,      xK_q        ), io (exitWith ExitSuccess))
    , ((modMask,                    xK_q        ), spawn "xmonad --recompile && xmonad --restart")
    ]

    ++
    [((m .|. modMask, k), windows $ f i)
        | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9]
        , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]
    ++
    [((m .|. modMask, key), screenWorkspace sc >>= flip whenJust (windows . f))
        | (key, sc) <- zip [xK_w, xK_e] [1, 0]
        , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]

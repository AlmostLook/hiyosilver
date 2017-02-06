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
import XMonad.Layout.NoBorders (noBorders, smartBorders)
import XMonad.Layout.PerWorkspace (onWorkspace, onWorkspaces)
import XMonad.Layout.Reflect (reflectHoriz)
import XMonad.Layout.IM
import XMonad.Layout.SimpleFloat
import XMonad.Layout.Spacing
import XMonad.Layout.ResizableTile
import XMonad.Layout.LayoutHints
import XMonad.Layout.LayoutModifier
import XMonad.Layout.Grid
import XMonad.Layout.ToggleLayouts
import Data.Ratio ((%))
import qualified XMonad.StackSet as W
import qualified Data.Map as M

myTerminal      = "urxvt"
modMask' :: KeyMask
modMask' = mod4Mask
myWorkspaces    = ["I" , "II", "III"] ++ ["IV", "V"]
myXmonadBar = "dzen2 -x '0' -y '0' -h '25' -w '450' -ta 'l' -fn '-*-inconsolata-*-r-normal-*-*-140-*-*-*-*-iso8859-*' -fg '#ffffff' -bg '#1B1D1E' -e 'button'"
myStatusBar = "conky -c /home/n3w4x/.xmonad/.conky_dzen | dzen2 -x '450' -y '0' -w '1240' -h '25' -ta 'r' -fn '-*-inconsolata-*-r-normal-*-*-140-*-*-*-*-iso8859-*' -bg '#1B1D1E' -fg '#ffffff' -e 'button'"
myBitmapDir = "/home/n3w4x/.xmonad/dzen2"

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
      , normalBorderColor   = "#000000"
      , focusedBorderColor  = "#ff5f00" 
      , borderWidth         = 1
      , startupHook         = setWMName "LG3D"
}

myManageHook :: ManageHook
myManageHook = composeAll
                [ className =? "urxvt"			--> doShift "I"
                , className =? "Chromium"		--> doShift "II"
                , className =? "libreoffice --writer"	--> doShift "III"
                , className =? "trayer"			--> doIgnore
		, className =? "steam"			--> doCenterFloat
                , className =? "manaplus"		--> doCenterFloat
		, isFullscreen				--> (doF W.focusDown <+> doFullFloat)
                , manageDocks]

myDoFullFloat :: ManageHook
myDoFullFloat = doF W.focusDown <+> doFullFloat

layoutHook'  =  onWorkspaces ["1"] customLayout $ 
                customLayout2

myLogHook :: Handle -> X ()
myLogHook h = dynamicLogWithPP $ defaultPP
    {
        ppCurrent           =   dzenColor "#ff5f00" "#1B1D1E" . pad
      , ppVisible           =   dzenColor "white" "#1B1D1E" . pad
      , ppHidden            =   dzenColor "white" "#1B1D1E" . pad
      , ppHiddenNoWindows   =   dzenColor "#7b7b7b" "#1B1D1E" . pad
      , ppUrgent            =   dzenColor "black" "red" . pad
      , ppWsSep             =   ""
      , ppSep               =   " "
      , ppLayout            =   dzenColor "#ff5f00" "#1B1D1E" .
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

mXPConfig :: XPConfig
mXPConfig =
    defaultXPConfig { font                  = "xft:inconsolata-16"
 
                    , bgColor               = "#2b2d2e"
                    , fgColor               = "#ff5f00"
                    , bgHLight              = "#ff5f00"
                    , fgHLight              = "#1B1D1E"
                    , promptBorderWidth     = 0
                    , height                = 17
                    , historyFilter         = deleteConsecutive
                    }
 
largeXPConfig :: XPConfig
largeXPConfig = mXPConfig
                { font = "xft:inconsolata-16"
                , height = 17
                }

keys' conf@(XConfig {XMonad.modMask = modMask}) = M.fromList $
    [ ((modMask,                    xK_p        ), runOrRaisePrompt largeXPConfig)
    , ((modMask .|. shiftMask,      xK_Return   ), spawn $ XMonad.terminal conf)    
    , ((modMask .|. shiftMask,      xK_l        ), spawn "xscreesaver-command -lock")
    , ((modMask,		            xK_c        ), spawn "chromium")
    , ((modMask,                    xK_s	    ), spawn "scrot.sh")
    , ((modMask,                    xK_x        ), spawn "thunar")
    , ((modMask,					xK_F2		), spawn "gmrun")
	, ((modMask,					xK_o		), spawn "/usr/bin/libreoffice --writer")
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
    , ((modMask .|. shiftMask,      xK_c        ), kill)
    , ((modMask .|. shiftMask,      xK_q        ), io (exitWith ExitSuccess))
    , ((modMask,                    xK_q        ), spawn "pkill conky && xmonad --recompile && xmonad --restart")
    ]

    ++
    [((m .|. modMask, k), windows $ f i)
        | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9]
        , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]
    ++
    [((m .|. modMask, key), screenWorkspace sc >>= flip whenJust (windows . f))
        | (key, sc) <- zip [xK_w, xK_e] [1, 0]
        , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]

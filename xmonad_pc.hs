--MY PERSONAL CONFIG--
import XMonad
import XMonad.Prompt --menú instrucciones gráficas para xmonad. modmask+p, no incluye shell
import XMonad.Prompt.RunOrRaise (runOrRaisePrompt) --abrir menu modmask+p
import XMonad.Prompt.AppendFile (appendFilePrompt) --gestor de notas modmask+control+n
import XMonad.Operations -- operaciones con ventanas, matar ventana, refrescar, ...

import System.IO --IO xmonad, ficheros y handles
import System.Exit --Salir programas, flushear handles, Exception, ExitCode
import XMonad.Util.Run --Comandos para Dzen, Dmenu, Xterm
import XMonad.Actions.CycleWS --focus ventanas, moverse entre ellas
import XMonad.Hooks.ManageDocks --avoidStruts, Dzen, xmobar, gnome-panel
import XMonad.Hooks.ManageHelpers --auxiliar ManageDocks, floating, fullscreen
import XMonad.Hooks.SetWMName --WM name=LG3D use Java 1.6u1, compatibilidad GUI Java 
import XMonad.Hooks.DynamicLog --statusBar Xmonad y Dzen
import XMonad.Hooks.UrgencyHook --para acciones cuando ventana requiere atención
import XMonad.Hooks.FadeInactive --opacidad ventanas inactivas -  xcompmgr 
import XMonad.Hooks.EwmhDesktops --EWMH para X
import XMonad.Layout.NoBorders (noBorders, smartBorders) --quitar bordes a pantalla completa
import XMonad.Layout.PerWorkspace (onWorkspace, onWorkspaces) --diferenciar layouts en workspaces
import XMonad.Layout.Reflect (reflectHoriz) --refleccion horizantal en layouts
import XMonad.Layout.IM --ajuste layout para Psi o Tkabber
import XMonad.Layout.SimpleFloat --diseño flotante basico
import XMonad.Layout.Spacing --espacio configurable alrededor de los layouts
import XMonad.Layout.ResizableTile --formato mosaico que permite cambiar altura/anchura de layouts
import XMonad.Layout.LayoutHints --respetar el tamaño
import XMonad.Layout.LayoutModifier --piratear layouts con modificaciones extras
import XMonad.Layout.Grid --diseño de layout que pone todas las ventanas en una misma cuadricula
import XMonad.Layout.ToggleLayouts --modulo para alternar entre los layouts
import Data.Ratio ((%)) --modulo operaciones matematicas con numeros racionales
import qualified XMonad.StackSet as W --workspaces y focos
import qualified Data.Map as M --diccionarios

myTerminal = "urxvt"
myModMask :: KeyMask
myModMask = mod4Mask
myWorkspaces = ["^i(" ++ myBitmapDir ++ "/arch_10x10.xbm)" , "II", "III"] ++ ["IV", "V"]
myXmonadBar = "dzen2 -x '0' -y '0' -h '25' -w '450' -ta 'l' -fn '-*-inconsolata-*-r-normal-*-*-140-*-*-*-*-iso8859-*' -fg '#ffffff' -bg '#1B1D1E' -e 'button'"
myStatusBar = "conky -c /home/n3w4x/.xmonad/.conky_dzen | dzen2 -x '450' -y '0' -w '1240' -h '25' -ta 'r' -fn '-*-inconsolata-*-r-normal-*-*-140-*-*-*-*-iso8859-*' -bg '#1B1D1E' -fg '#ffffff' -e 'button'"
myBitmapDir = "/home/n3w4x/.xmonad/dzen2"

main = do
    dzenLeftBar <- spawnPipe myXmonadBar
    dzenRightBar <- spawnPipe myStatusBar
    xmonad $ withUrgencyHookC dzenUrgencyHook { args = ["-bg", "red", "fg", "black", "-xs", "1", "-y", "25"] } urgencyConfig { remindWhen = Every 5 } $ ewmh defaultConfig
      { terminal            = myTerminal
      , workspaces          = myWorkspaces
      , keys                = myKeys
      , modMask             = myModMask
      , layoutHook          = myLayoutHook
      , manageHook          = manageDocks <+> myManageHook <+> manageHook defaultConfig
      , logHook             = myLogHook dzenLeftBar >> fadeInactiveLogHook 0xdddddddd
      , normalBorderColor   = "#000000"
      , focusedBorderColor  = "#ff5f00" 
      , borderWidth         = 1
      , startupHook         = setWMName "LG3D"
}

myManageHook :: ManageHook
myManageHook = composeAll
                [ className =? "Chromium"               --> doShift "II"
                , className =? "Firefox"                --> doShift "II"
                , className =? "libreoffice-writer"     --> doShift "III"
                , className =? "Pidgin"                 --> doShift "V"
                , className =? "vlc"                    --> doFloat
                , className =? "Steam"                  --> doCenterFloat
                , className =? "Tomboy"                 --> doCenterFloat
                , className =? "Keepas"                 --> doCenterFloat
                , className =? "trayer"                 --> doIgnore
                , isFullscreen                          --> (doF W.focusDown <+> doFullFloat)
                , manageDocks]

myDoFullFloat :: ManageHook
myDoFullFloat = doF W.focusDown <+> doFullFloat

myLayoutHook  =  onWorkspaces ["I"] customLayout $ customLayout2

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

myKeys conf@(XConfig {XMonad.modMask = modMask}) = M.fromList $
--TECLAS DE PROGRAMAS
    [ ((modMask .|. controlMask,    xK_F1       ), appendFilePrompt def "/home/n3w4x/notas.txt")
    , ((modMask .|. controlMask,    xK_F11      ), spawn "xdotool_repeat_key.sh")
    , ((modMask .|. controlMask,    xK_F12      ), spawn "pkill xdotool")
    , ((modMask .|. controlMask,    xK_Return   ), spawn $ XMonad.terminal conf)    
    , ((modMask .|. controlMask,    xK_c        ), spawn "chromium about:blank")
    , ((modMask .|. controlMask,    xK_f        ), spawn "firefox -private-window")
    , ((modMask .|. controlMask,    xK_s        ), spawn "scrot.sh")
    , ((modMask .|. controlMask,    xK_t        ), spawn "thunar")
    , ((modMask .|. controlMask,    xK_F2       ), spawn "gmrun")
    , ((modMask .|. controlMask,    xK_o        ), spawn "libreoffice --writer")
    , ((modMask .|. controlMask,    xK_r        ), spawn "urxvt -e ncmpcpp")
    , ((modMask .|. controlMask,    xK_m        ), spawn "urxvt -e mutt")
    , ((modMask .|. controlMask,    xK_k        ), spawn "keepass")
    , ((modMask .|. controlMask,    xK_w        ), spawn "whatsie")
    , ((modMask .|. controlMask,    xK_n        ), spawn "mousepad")
--TECLAS INTERNAS
    ,((modMask,                     xK_p        ), runOrRaisePrompt largeXPConfig)
    , ((modMask .|. shiftMask,      xK_l        ), spawn "slock")
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
    , ((modMask,                    xK_h        ), sendMessage Shrink)
    , ((modMask,                    xK_l        ), sendMessage Expand)
    , ((modMask .|. shiftMask,      xK_h        ), sendMessage MirrorShrink)
    , ((modMask .|. shiftMask,      xK_l        ), sendMessage MirrorExpand)
    , ((modMask,                    xK_comma    ), sendMessage (IncMasterN 1))
    , ((modMask,                    xK_period   ), sendMessage (IncMasterN (-1)))
    , ((modMask,                    xK_i        ), sendMessage ToggleLayout)
    , ((modMask,                    xK_Right    ), nextWS)
    , ((modMask .|. shiftMask,      xK_Right    ), shiftToNext)
    , ((modMask,                    xK_Left     ), prevWS)
    , ((modMask .|. shiftMask,      xK_Left     ), shiftToPrev)
    , ((modMask .|. shiftMask,      xK_r        ), spawn "sleep 3 && systemctl reboot")
    , ((modMask .|. shiftMask,      xK_c        ), kill)
    , ((modMask .|. shiftMask,      xK_q        ), io (exitWith ExitSuccess))
    , ((modMask,                    xK_q        ), spawn "pkill conky && killall dzen2 && xmonad --recompile && xmonad --restart")
    ]

    ++
    [((m .|. modMask, k), windows $ f i)
        | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9]
        , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]
    ++
    [((m .|. modMask, key), screenWorkspace sc >>= flip whenJust (windows . f))
        | (key, sc) <- zip [xK_w, xK_e] [1, 0]
        , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]

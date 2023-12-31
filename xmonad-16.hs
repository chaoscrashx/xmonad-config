import XMonad
import XMonad.Config.Desktop
import XMonad.Config.Gnome
import XMonad.Config.Mate
import qualified XMonad.StackSet as W
import qualified Data.Map        as M
import qualified DBus as D
import qualified DBus.Client as D
import qualified Codec.Binary.UTF8.String as UTF8

import Data.Monoid (All(All), mappend)

import XMonad.Actions.CycleWS
import XMonad.Actions.FlexibleResize as Flex
import XMonad.Actions.Search
import XMonad.Actions.UpdatePointer
import XMonad.Actions.WindowGo
import XMonad.Actions.GridSelect
import qualified XMonad.Actions.Submap as SM


import XMonad.Hooks.DynamicLog
--import XMonad.Hooks.EwmhDesktops (ewmh,fullscreenEventHook, ewmhDesktopsLogHookCustom)
-- import XMonad.Hooks.EwmhDesktops 
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.SetWMName
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.UrgencyHook
import XMonad.Hooks.ScreenCorners

import XMonad.Layout.BorderResize
import XMonad.Layout.LayoutHints
import XMonad.Layout.SimpleDecoration
import XMonad.Layout.IM
import XMonad.Layout.NoBorders
import XMonad.Layout.ResizableTile
import XMonad.Layout.Simplest
import XMonad.Layout.Decoration
import XMonad.Layout.ToggleLayouts
import XMonad.Layout.WindowArranger
import XMonad.Layout.Mosaic
import XMonad.Layout.Circle
import XMonad.Layout.Grid
import XMonad.Layout.SimpleFloat
import XMonad.Layout.SimplestFloat
import XMonad.Layout.Spiral
import XMonad.Layout.Tabbed
import XMonad.Layout.ThreeColumns
import XMonad.Layout.Accordion
import XMonad.Layout.Named         -- custom layout names
import XMonad.Layout.DecorationAddons
import XMonad.Layout.ButtonDecoration
import XMonad.Layout.Decoration
import XMonad.Layout.Maximize
import XMonad.Layout.Minimize
import XMonad.Layout.NoFrillsDecoration
import XMonad.Layout.MouseResizableTile -- for mouse control
import qualified XMonad.Layout.Magnifier as Mag
import XMonad.Layout.DecorationMadness
import XMonad.Actions.MouseResize
import XMonad.Layout.WindowArranger


import XMonad.Prompt
import XMonad.Prompt.Layout
import XMonad.Prompt.Input
import XMonad.Prompt.RunOrRaise
import XMonad.Prompt.Shell
import XMonad.Prompt.Window

import XMonad.Util.Run
import XMonad.Util.Font
import XMonad.Util.Themes
import XMonad.Util.Scratchpad
import XMonad.Util.WorkspaceCompare
import XMonad.Util.EZConfig
-- import Graphics.X11.ExtraTypes.XF86

--import System.Taffybar.Support.PagerHints (pagerHints)

import System.Environment
-- import System.Cmd
import System.IO
import System.Exit
import Control.Concurrent

myKeys conf@(XConfig {XMonad.modMask = modMask}) = M.fromList $

    -- terminals
    [ ((modMask,                 xK_Return), spawn $ XMonad.terminal conf)
    -- ((modMask,                 xK_Return), spawn $ XMonad.terminal conf)
    , ((modMask .|. shiftMask,   xK_Return), spawn "kgx")

    -- launcher
    , ((modMask .|. shiftMask,   xK_p), spawn "gmrun")

    -- file manager
    , ((modMask ,                 xK_Up    ), raiseMaybe (spawn "nautilus ~") (className =? "Org.gnome.Nautilus" <&&> title =? "Home"))
    --, ((modMask,                 xK_Up    ), runOrRaise "nautilus ~" (className =? "Nautilus"))
    --, ((modMask .|. shiftMask,   xK_Up    ), spawn "nautilus ~")
    --, ((modMask,                 xK_Up    ), spawn "nautilus ~")


    -- shell/window prompts
    , ((modMask,                 xK_F2 ), runOrRaisePrompt mySP)

    -- Volume keys
    -- , ((0, 0x1008ff11), spawn "amixer -q set Master 5%-")
    -- , ((0, 0x1008ff13), spawn "amixer -q set Master 5%+")
    -- , ((0, 0x1008ff12), spawn "amixer -q set Master toggle")

    -- browser
    , ((modMask,               xK_f     ), raiseNextMaybe (spawn "firefox") (className =? "firefox"))
        -- , ((modMask,               xK_f     ), raiseMaybe
        -- "firefox" (className =? "Firefox"))
     -- browser
    , ((modMask,               xK_c     ), raiseNextMaybe (spawn "google-chrome-stable") (className =? "Google-chrome"))

    -- print screen
    , ((0,                     xK_Print ), unsafeSpawn "scrot -e 'mv $f ~/Pictures'")


    -- cycle through workspaces
    , ((modMask,               xK_Right ), moveTo Next (WSIs (return $ not . (=="SP") . W.tag)))
    , ((modMask,               xK_Left  ), moveTo Prev (WSIs (return $ not . (=="SP") . W.tag)))

    -- move windows through workspaces
    , ((modMask .|. shiftMask, xK_Right ), shiftTo Next (WSIs (return $ not . (=="SP") . W.tag)))
    , ((modMask .|. shiftMask, xK_Left  ), shiftTo Prev (WSIs (return $ not . (=="SP") . W.tag)))
    , ((modMask .|. controlMask, xK_Right), shiftTo Next emptyWS)
    , ((modMask .|. controlMask, xK_Left), shiftTo Prev emptyWS)

    -- Rotate through layouts
    , ((modMask,               xK_space ), sendMessage NextLayout)


    -- Move focus to the next/previous window
    , ((modMask,               xK_j     ), windows W.focusDown)
    , ((modMask,               xK_Tab   ), windows W.focusDown)
    , ((mod1Mask,              xK_Tab   ), spawn "$HOME/.xmonad/random_window_switcher.sh" )
    -- , ((mod1Mask,              xK_Tab   ), windows W.focusDown)
    , ((modMask,               xK_k     ), windows W.focusUp)
    , ((modMask .|. shiftMask, xK_Tab   ), windows W.focusUp)
    , ((mod1Mask .|. shiftMask, xK_Tab  ), windows W.focusUp)

    -- Swap the focused window with next/prev window
    , ((modMask .|. shiftMask, xK_j     ), windows W.swapDown)
    , ((modMask .|. shiftMask, xK_k     ), windows W.swapUp)

    -- Shrink/Expand the master area
    , ((modMask,               xK_h     ), sendMessage Shrink)
    , ((modMask,               xK_l     ), sendMessage Expand)
    , ((modMask .|. shiftMask, xK_h     ), sendMessage MirrorShrink)
    , ((modMask .|. shiftMask, xK_l     ), sendMessage MirrorExpand)

    -- Swap the focused window and the master window
    , ((modMask,            xK_semicolon), windows W.swapMaster)

    -- Increment/Deincrement the number of windows in the master area
    , ((modMask,               xK_comma ), sendMessage (IncMasterN 1))
    , ((modMask,               xK_period), sendMessage (IncMasterN (-1)))

    -- Resize viewed windows to the correct size
    , ((modMask,               xK_n     ), refresh)
    -- Reset layout of current workspace
    , ((modMask .|. shiftMask, xK_n     ), setLayout $ XMonad.layoutHook conf)


    -- Mosaic
    , ((modMask , xK_a                  ), sendMessage Taller)
    , ((modMask , xK_z                  ), sendMessage Wider)
    , ((modMask .|. controlMask, xK_n   ), sendMessage Reset)

    --Grid Select
    , ((mod1Mask , xK_g            ), goToSelected  $ gsconfig1 )
    , ((modMask , xK_g            ), spawn "$HOME/.xmonad/random_menu.sh" )
    --, ((mod1Mask , xK_g            ), spawn  "rofi -show combi" )

    -- toggle focused window fullscreen
    , ((modMask,               xK_m     ), sendMessage (Toggle "Full"))

    --- Maginifier toggles 
    , ((modMask .|. controlMask .|. shiftMask, xK_minus ), sendMessage Mag.MagnifyMore)
    , ((modMask .|. controlMask, xK_minus), sendMessage Mag.MagnifyLess)
    , ((modMask .|. shiftMask, xK_m     ), sendMessage Mag.Toggle)
    -- Push window back into tiling
    , ((modMask,               xK_t     ), withFocused $ windows . W.sink)
    , ((modMask .|. shiftMask, xK_s     ), sendMessage Arrange)

    -- toggle the status bar gap
    , ((modMask,               xK_b     ), sendMessage ToggleStruts)

    -- close focused window
    , ((modMask .|. shiftMask,  xK_c     ), kill)

    -- Restart xmonad
    , ((modMask              , xK_q     ),
        broadcastMessage ReleaseResources >> restart "xmonad" True)

 -- Logout of  xmonad
    , ((modMask .|. shiftMask , xK_q     ),sequence_ [io (exitWith ExitSuccess), spawn "mate-session-save --kill"])
    ]

    ++
    -- mod-[1..9], Switch to workspace N
    -- mod-shift-[1..9], Move client to workspace N
    [ ((m .|. modMask, k), windows $ f i)
        | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9]
        , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)] ]

    ++
    -- mod-{w,e,r}, Switch to physical/Xinerama screens 1, 2, or 3
    -- mod-shift-{w,e,r}, Move client to screen 1, 2, or 3
    [((m .|. modMask, key), screenWorkspace sc >>= flip whenJust (windows . f))
        | (key, sc) <- zip [xK_w, xK_e, xK_r] [0..]
        , (f, m) <- [(W.view, 0), (W.shift, shiftMask)] ]

myMouseBindings (XConfig {XMonad.modMask = modMask}) = M.fromList $
    -- mod-button1, Set the window to floating mode and move by dragging
    [ ((modMask, button1), (\w -> focus w >> mouseMoveWindow w))
    -- mod-button2, Raise the window to the top of the stack
    , ((modMask, button2), (\w -> focus w >> windows W.swapMaster))
    -- mod-button3, Set the window to floating mode and resize by dragging
    , ((modMask, button3), (\w -> focus w >> Flex.mouseResizeWindow w)) ]



-- Tags/Workspaces
-- clickable workspaces via dzen/xdotool
myWorkspaces            :: [String]
myWorkspaces            = (map dzenEscape) $ ["1","2","3","4","5","6","7","8","9"]


-- shell prompt theme
mySP = def
    { bgColor           = "black"
    , fgColor           = "white"
    , bgHLight          = "gray"
    , fgHLight          = "black"
    , borderColor       = "orange"
    , promptBorderWidth = 1
    , position          = Bottom
    , height            = 20
    --, autoComplete      = Just 1000
    , historySize       = 1000 }

theFont :: String
theFont = "xft:Monospace-13"
-- simpleFloat def {fontName = theFont} |||


-- layouts 
--myLayout = tiled ||| Circle ||| simpleFloat' shrinkText myTab ||| ThreeColMid nmaster (delta) (ratio) ||| Grid |||  layoutHints (tabbed shrinkText myTab) |||  spiral (6/7)  ||| mosaic 2 [3,2]
myLayout = named "Tall" tiled |||  named "Grid" grid1 |||  mouseResizableTile ||| named "Circle" circle1 |||  named "Float" float1 |||  named "Tabbed" tab1 |||  Accordion ||| named "Mosaic" mosaic1 ||| named "ThreeCol" threecol1 
    where
        tiled   = Mag.magnifierOff( ResizableTall nmaster delta ratio [])
        nmaster = 1
        delta   = 3/100
        ratio   = 1/2
        -- float1 = maximize $ buttonDeco shrinkText myTab  (simplestFloat)
        -- float2 = floatDwmStyle shrinkText myTab 
        -- float1 =   borderResize (buttonDeco shrinkText tabThemeWithButtons (floatDwmStyle shrinkText tabThemeWithButtons))
        float1 =   borderResize $ mouseResize $ (buttonDeco shrinkText tabThemeWithButtons (simplestFloat))
        -- float2 = minimize $ maximize $ buttonDeco shrinkText myTab (simplestFloat)  (theme adwaitaDarkTheme)
        tab1 = layoutHints (tabbed shrinkText myTab)
        grid1 =  Mag.magnifier Grid
        mosaic1 = Mag.magnifier (mosaic 2 [3,2])
        threecol1 = Mag.magnifier (ThreeColMid nmaster delta ratio)
        circle1 = Mag.magnifierOff(Circle)

-- special windows
myManageHook = composeAll
    [ className =? "MPlayer"        --> doFloat
    , className =? "Gimp"           --> doFloat
    , className =? "Pidgin"         --> doFloat
    , className =? "Empathy"         --> doFloat
--    , className =? "Gnome-terminal"         -->  doShift (myWorkspaces !! 8)
    , className =? "Gnome-calculator"         --> doFloat
    , title     =? "glxgears"       --> doFloat
    , title     =? "inferno"        --> doFloat
    , title     =? "Contact List"   --> doFloat
    , title     =? "Downloads"   --> doFloat
    , title     =? "Save As..."   --> doFloat
    , className =? "Empathy"        --> doFloat
    , className =? "Gnome-panel"    --> doIgnore
    , className =? "XVkbd"          --> doIgnore
    , className =? "Cellwriter"     --> doIgnore
    , className =? "Gtkdialog"      --> doFloat
    , resource  =? "desktop_window" --> doIgnore
    , isFullscreen             --> doFullFloat
    --                                      x y w h
    , scratchpadManageHook $ W.RationalRect 0 0 1 0.42
    , manageDocks ] <+> manageHook def

-- Grid Select Section
--gsconfig2 colorizer = (buildDefaultGSConfig colorizer) { gs_cellheight = 60 ,gs_cellwidth = 300, gs_font = "xft:Noto Sans:pixelsize=18",gs_cellpadding = 5 }
gsconfig1  = def  { gs_cellheight = 60 ,gs_cellwidth = 250, gs_font = "xft:Noto Sans:pixelsize=20",gs_cellpadding = 5 }


-- | A green monochrome colorizer based on window classimport XMonad.Layout.IM
myColorizer = colorRangeFromClassName
                      (0x57,0xFF,0x1F) -- lowest inactive bg
                      (0x24,0x57,0xFF) -- highest inactive bg
                      (0x52,0x37,0xA4) -- active bg
                      black            -- inactive fg
                      white            -- active fg
   where black = minBound
         white = maxBound




appFontXft :: String
appFontXft = "xft:Noto Mono Regular:pixelsize=12"
-- appFontXft = "xft:Noto Sans:pixelsize=12"


-- Color of current window title in xmobar.--#FFB6B0
xmobarTitleColor = "red"

-- Color of current workspace in xmobar.
xmobarCurrentWorkspaceColor = "red"



-- decoration theme
myDeco = def
    { activeColor         = "orange"
    , inactiveColor       = "#222222"
    , urgentColor         = "yellow"
    , activeBorderColor   = "orange"
    , inactiveBorderColor = "#222222"
    , urgentBorderColor   = "yellow"
    , activeTextColor     = "orange"
    , inactiveTextColor   = "#222222"
    , urgentTextColor     = "yellow"
    , decoHeight          = 10 }


-- myEventHook e = do
--         screenCornerEventHook e


myStartupHook = do
--        spawn "/usr/libexec/gsd-xsettings"
--        spawn "/usr/libexec/gsd-media-keys"
--        spawn "/usr/libexec/gnome-fallback-mount-helper"
--        spawn "/usr/bin/gnome-sound-applet"
--        spawn "/usr/bin/nm-applet"
        spawn "/usr/bin/synapse"
--        spawn "/usr/bin/start-pulseaudio-x11"
--        spawn "/usr/bin/gsettings-data-convert"
--        spawn "/usr/bin/xdg-user-dirs-gtk-update"
--        spawn "/usr/bin/trayer-srg --edge top --align right --SetDockType true --SetPartialStrut true --expand true --width 230 --widthtype pixel  --transparent true --height 22"
        spawn "/usr/bin/compton"
--        spawn "/usr/bin/gnome-keyring-daemon --start --components=gpg,pkcs11,secrets,ssh"
--        spawn "/usr/bin/xfce4-power-manager"
--        spawn "/usr/bin/caffeine-indicator"
--        spawn "/usr/lib/x86_64-linux-gnu/polkit-mate/polkit-mate-authentication-agent-1"
--        spawn "/usr/bin/kdeconnect-indicator"
--        spawn "/usr/bin/dunst"
--        spawn "/usr/bin/synclient TapButton3=2"
        spawn "xsetroot -cursor_name left_ptr"
--        spawn "/home/max/scriptz/random_wallpaper.sh"

minimizeButtonOffset :: Int
minimizeButtonOffset = 48

maximizeButtonOffset :: Int
maximizeButtonOffset = 25

closeButtonOffset :: Int
closeButtonOffset = 10

buttonSize :: Int
buttonSize = 10


tabThemeWithButtons :: Theme
tabThemeWithButtons = myTab {
                            windowTitleAddons = [ ("(M)", AlignLeft)
                                                , ("-"   , AlignRightOffset minimizeButtonOffset)
                                                , ("[]"  , AlignRightOffset maximizeButtonOffset)
                                                , ("X"   , AlignRightOffset closeButtonOffset)
                                                ]
                            }

-- tab theme
myTab = def
    { activeColor         = "black"
    , inactiveColor       = "black"
    , urgentColor         = "yellow"
    , activeBorderColor   = "orange"
    , inactiveBorderColor = "#222222"
    , urgentBorderColor   = "black"
    , activeTextColor     = "orange"
    , fontName            = "xft:Noto Mono Regular:pixelsize=14"
    , inactiveTextColor   = "white"
    , decoHeight          = 24
    , decoWidth           = 24
    , urgentTextColor     = "yellow" }

-- myLogHook h = ewmhDesktopsLogHookCustom scratchpadFilterOutWorkspace >> updatePointer (0.5, 0.5) (1, 1)
--myLogHook h = dynamicLogWithPP $ myDzenPP { ppOutput = hPutStrLn h }





--dbus stuff for mate panel integration with xmonad-log-applet




prettyPrinter :: D.Client -> PP
prettyPrinter dbus = def
    { ppOutput   = dbusOutput dbus
    , ppTitle    = pangoColor "cyan" . pangoSanitize
    , ppCurrent  = pangoColor "cyan" . wrap "[" "]" . pangoSanitize
    , ppVisible  = pangoColor "orange" . wrap "(" ")" . pangoSanitize
    , ppHiddenNoWindows   = pangoColor "white"
    , ppHidden  = pangoColor "#78C209"
    , ppSort = getSortByXineramaRule
    , ppUrgent   = pangoColor "red"
    , ppLayout  = pangoColor "cyan".wrap "| " " |" . pangoSanitize
    , ppSep      = "   "
    }




myDzenPP  = dzenPP
    { ppCurrent = dzenColor "#00d5ff" "" . wrap " " " "
    , ppHidden  = dzenColor "#C7F09F" "" . wrap " " " "
    , ppHiddenNoWindows = dzenColor "#ff5500" "" . wrap " " " "
    , ppUrgent  = dzenColor "#ff0000" "" . pad . dzenStrip
    , ppSep     = "     "
    , ppVisible = wrap "[" "]"
    , ppSort = getSortByXineramaRule
    , ppLayout  = dzenColor "#4811a4" "#ff5500" . wrap "^ca(1,xdotool key super+space)· " " ·^ca()"
    , ppTitle   = dzenColor "#ff5500" "#222222"
                    . wrap "^ca(1,xdotool key super+k)^ca(2,xdotool key super+shift+c)"
                           "                          ^ca()^ca()" . dzenEscape
    }






getWellKnownName :: D.Client -> IO ()
getWellKnownName dbus = do
  D.requestName dbus (D.busName_ "org.xmonad.Log")
                [D.nameAllowReplacement, D.nameReplaceExisting, D.nameDoNotQueue]
  return ()
  
dbusOutput :: D.Client -> String -> IO ()
dbusOutput dbus str = do
    let signal = (D.signal (D.objectPath_ "/org/xmonad/Log") (D.interfaceName_ "org.xmonad.Log") (D.memberName_ "Update")) {
            D.signalBody = [D.toVariant ("<b>" ++ (UTF8.decodeString str) ++ "</b>")]
        }
    D.emit dbus signal

pangoColor :: String -> String -> String
pangoColor fg = wrap left right
  where
    left  = "<span foreground=\"" ++ fg ++ "\">"
    right = "</span>"

pangoSanitize :: String -> String
pangoSanitize = foldr sanitize ""
  where
    sanitize '>'  xs = "&gt;" ++ xs
    sanitize '<'  xs = "&lt;" ++ xs
    sanitize '\"' xs = "&quot;" ++ xs
    sanitize '&'  xs = "&amp;" ++ xs
    sanitize x    xs = x:xs



-- "-*-noto sans-medium-r-normal-*-12-120-*-*-p-*-iso8859-*"


myDzenStatus = "dzen2 -w '930' -ta 'l'" ++ myDzenStyle
myDzenConky  = "conky -c ~/.xmonad/conkyrc | dzen2 -x '930' -w '760' -ta 'r'" ++ myDzenStyle
-- myDzenStyle  = " -h '22' -fg '#777777' -bg '#222222' -fn '-monotype-noto sans mono medium-medium-r-normal--13-120-0-0-m-0-iso8859-1'"
myDzenStyle  = " -h '22' -fg '#777777' -bg '#222222' -fn '-monotype-noto sans-medium-*-normal-*-100-100-*-*-iso8859-*'"
--myStartMenu = "/home/roh/.xmonad/start /home/roh/.xmonad/start_apps"

mySort = getSortByXineramaRule


----status <- spawnPipe myDzenStatus    -- xmonad status on the left
        --conky  <- spawnPipe myDzenConky     -- conky stats on the right
--    dzenStartMenu    <- spawnPipe myStartMenu

main :: IO ()
main = do
    dbus <- D.connectSession
    getWellKnownName dbus
    xmonad $   ewmhFullscreen . setEwmhWorkspaceSort mySort. ewmh   $ withUrgencyHook dzenUrgencyHook $ mateConfig {
                 terminal           = "tilix"
                           , borderWidth        = 1
                           , normalBorderColor  = "black"
                           , focusedBorderColor = "orange"
                           , focusFollowsMouse  = True
                           , modMask            = mod4Mask
                           , keys               = myKeys
               , mouseBindings      = myMouseBindings
               , workspaces = myWorkspaces
               , startupHook    =  myStartupHook >>  setWMName "LG3D"
                           , layoutHook         =  smartBorders $ avoidStruts $ desktopLayoutModifiers $ toggleLayouts (noBorders Full)  myLayout
                           , manageHook         =  manageDocks <+> myManageHook <+> manageHook desktopConfig
                           , handleEventHook    =  handleEventHook desktopConfig
                           , logHook         =  do
                               dynamicLogWithPP (prettyPrinter dbus)
                               updatePointer (0.5, 0.5) (1, 1)
                               logHook desktopConfig                              

               }

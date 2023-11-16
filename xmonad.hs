import XMonad
import XMonad.Config.Desktop
import XMonad.Config.Gnome
--import XMonad.Config.Mate
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
import XMonad.Actions.EasyMotion (selectWindow, EasyMotionConfig(..) )
import qualified XMonad.Actions.Submap as SM


import XMonad.Hooks.DynamicLog
--import XMonad.Hooks.EwmhDesktops (ewmh,fullscreenEventHook, ewmhDesktopsLogHookCustom)
-- import XMonad.Hooks.EwmhDesktops 
import XMonad.Hooks.EwmhDesktops
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
import XMonad.Layout.Dishes
import XMonad.Layout.Maximize
import XMonad.Layout.Minimize
import XMonad.Layout.NoFrillsDecoration
import XMonad.Layout.MouseResizableTile -- for mouse control
import qualified XMonad.Layout.Magnifier as Mag
import XMonad.Layout.DecorationMadness
import XMonad.Actions.MouseResize
import XMonad.Layout.WindowArranger
import XMonad.Layout.PerWorkspace


import XMonad.Prompt
import XMonad.Prompt.Layout
import XMonad.Prompt.Input
import XMonad.Prompt.RunOrRaise
import XMonad.Prompt.Shell
import XMonad.Prompt.Window

import XMonad.Util.Run
import XMonad.Util.Paste
import XMonad.Util.Font
import XMonad.Util.Themes
import XMonad.Util.Scratchpad
import XMonad.Util.NamedScratchpad
import XMonad.Util.WorkspaceCompare
import XMonad.Util.EZConfig
import XMonad.Actions.SpawnOn
import XMonad.Util.NamedScratchpad (namedScratchpadFilterOutWorkspacePP,namedScratchpadManageHook, defaultFloating, namedScratchpadAction)



import System.Environment
import System.IO
import System.Exit
import Control.Concurrent

myKeys conf@(XConfig {XMonad.modMask = modMask}) = M.fromList $

    -- terminals
    [ ((modMask,                 xK_Return), spawn $ XMonad.terminal conf)
    , ((modMask .|. shiftMask,   xK_Return), spawn "kgx")

    -- launcher
    -- , ((modMask .|. shiftMask,   xK_p), spawn "gmrun")

    -- file manager
    , ((modMask ,                 xK_Up    ), raiseMaybe (spawn "nautilus ~") (className =? "Org.gnome.Nautilus" <&&> title =? "Home"))


    -- shell/window prompts
    , ((modMask,                 xK_F2 ), runOrRaisePrompt mySP)

   
    , ((0,                       xK_Insert), pasteSelection)



    -- browser
    , ((modMask,               xK_f     ), raiseNextMaybe (spawn "firefox") (className =? "firefox"))
    , ((modMask,               xK_c     ), raiseNextMaybe (spawn "code") (className =? "Code"))
    -- , ((modMask,               xK_c     ), raiseNextMaybe (spawn "google-chrome-stable") (className =? "Google-chrome"))
    , ((modMask,               xK_x     ), raiseNextMaybe (spawn "microsoft-edge-stable") (className =? "Microsoft-edge"))
    --  , ((modMask,   xK_x ), namedScratchpadAction myScratchPads "notes")
     , ((modMask,              xK_n ), namedScratchpadAction myScratchPads "notes")

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
    , ((mod1Mask,              xK_Tab   ), spawn "rofi -show combi" )
--, ((mod1Mask,              xK_Tab   ), spawn "/home/arthavah/.config/rofi/launchers/random_window_switcher.sh" )
    -- , ((mod1Mask,              xK_Tab   ), windows W.focusDown)
    , ((modMask,               xK_k     ), windows W.focusUp)
    , ((modMask .|. shiftMask, xK_Tab   ), windows W.focusUp)
    -- , ((mod1Mask .|. shiftMask, xK_Tab  ), windows W.focusUp)

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
    -- , ((modMask,               xK_n     ), refresh)
    -- Reset layout of current workspace
    , ((modMask .|. shiftMask, xK_n     ), setLayout $ XMonad.layoutHook conf)


    -- Mosaic
    , ((modMask , xK_a                  ), sendMessage Taller)
    , ((modMask , xK_z                  ), sendMessage Wider)
    , ((modMask .|. controlMask, xK_n   ), sendMessage Reset)

    --Grid Select
    , ((modMask , xK_g            ), goToSelected  $ gsconfig1 )
    , ((mod1Mask , xK_g            ), spawn "~/.config/rofi/scripts/random_launcher.sh" )
    --, ((mod1Mask , xK_g            ), spawn  "rofi -show combi" )

    -- toggle focused window fullscreen
    , ((modMask,               xK_m     ), sendMessage (Toggle "Full"))

    --- Maginifier toggles 
    -- , ((modMask .|. controlMask .|. shiftMask, xK_minus ), sendMessage Mag.MagnifyMore)
    , ((modMask .|. controlMask, xK_plus ), sendMessage Mag.MagnifyMore)
    , ((modMask .|. controlMask, xK_minus), sendMessage Mag.MagnifyLess)
    , ((modMask .|. shiftMask, xK_m     ), sendMessage Mag.Toggle)
    --  Push window back into tiling
    , ((modMask,               xK_t     ), withFocused $ windows . W.sink)
    -- , ((modMask .|. shiftMask, xK_s     ), sendMessage Arrange)

-- Easy Motion 

    , ((modMask .|. controlMask,  xK_s), selectWindow def{txtCol="Green", cancelKey=xK_Escape} >>= (`whenJust` windows . W.focusWindow))
    , ((modMask .|. shiftMask,  xK_s), selectWindow def{txtCol="Red",cancelKey=xK_Escape} >>= (`whenJust` killWindow))


    -- , ((modMask, xK_s), selectWindow def{txtCol="Green", cancelKey=xK_Escape, emFont="xft:Noto Sans Regular:pixelsize=80"} >>= (`whenJust` windows . W.focusWindow))
    -- , ((modMask .|. shiftMask, xK_s), selectWindow def{txtCol="Red",cancelKey=xK_Escape, emFont="xft:Noto Sans Regular:pixelsize=80"} >>= (`whenJust` killWindow))


    -- toggle the status bar gap
    , ((modMask,               xK_b     ), sendMessage ToggleStruts)

    -- close focused window
    , ((modMask .|. shiftMask,  xK_c     ), kill)

    -- Restart xmonad
    , ((modMask              , xK_q     ),
        broadcastMessage ReleaseResources >> restart "xmonad" True)

 -- Logout of  xmonad
    , ((modMask .|. shiftMask , xK_q     ),sequence_ [io (exitWith ExitSuccess), spawn "gnome-session-quit"])
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
-- myWorkspaces            = (map dzenEscape) $ ["1","2","3","4","5","6","7","8","9"]

myWorkspaces            = ["1","2","3","4","5","6","7","8","9"]

myTerminal :: String
myTerminal = "kgx "

myScratchPads :: [NamedScratchpad]
myScratchPads = [
    NS "terminal" spawnTerm findTerm manageTerm
    , NS "calculator" spawnCalc findCalc manageCalc
    ,NS "notes" spawnNotes findNotes manageNotes
    ]
    where
    spawnTerm  = myTerminal ++ " -T scratchpad"
    findTerm   = title =? "scratchpad"
    manageTerm = defaultFloating 
    spawnCalc  = "gnome-calculator"
    findCalc   = title =? "Calculator"
    manageCalc = defaultFloating
    spawnNotes  = "APPIMAGELAUNCHER_DISABLE=1 .joplin/Joplin.AppImage"
    findNotes   = className =? "Joplin"
    manageNotes = defaultFloating


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
    , autoComplete      = Just 1000
    , historySize       = 1000 }

theFont :: String
theFont = "xft:Noto Sans:pixelsize=20"


-- layouts 
myLayout = onWorkspaces ["1"]  grid1 (named "Tall" tiled)  ||| named "Tall" tiled |||  named "Grid" grid1 ||| named "Tabbed" tab1 ||| named "Circle" circle1  |||  mouseResizableTile |||  named "Float" float1  |||   Accordion ||| named "Mosaic" mosaic1 ||| named "ThreeCol" threecol1 
    where
        tiled   = Mag.magnifierOff( ResizableTall nmaster delta ratio [])
        nmaster = 1
        delta   = 3/100
        ratio   = 1/2
        float1 =   borderResize $ mouseResize $ (buttonDeco shrinkText tabThemeWithButtons (simplestFloat))
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
    , className =? "Tilix"         -->  doShift (myWorkspaces !! 0)
    , className =? "Gnome-terminal"         -->  doShiftAndGo (myWorkspaces !! 0)
    , className =? "firefox"     --> doShift (myWorkspaces !! 2)
    , className =? "microsoft-edge"     --> doShift (myWorkspaces !! 2)
    , className =? "Code"     --> doShift (myWorkspaces !! 1)
    , className =? "Codux"     --> doShift (myWorkspaces !! 1)
    , className =? "Chromium"     --> doShift (myWorkspaces !! 3)
    , className =? "Google-chrome"     --> doShift (myWorkspaces !! 3)
    , title     =? "glxgears"       --> doFloat
    , title     =? "inferno"        --> doFloat
    , title     =? "Contact List"   --> doFloat
    , title     =? "Downloads"   --> doFloat
    , title     =? "Save As"   --> doFloat
    , title 	=? "Panel Properties" --> doFloat
    , className =? "Empathy"        --> doFloat
    , className =? "Gnome-panel"    --> doIgnore
    , className =? "XVkbd"          --> doIgnore
    , className =? "Cellwriter"     --> doIgnore
    , className =? "confirm"        --> doFloat
    , className =? "Gtkdialog"      --> doFloat
    , className =? "file_progress"             --> doFloat
    , className =? "dialog"                    --> doFloat
    , className =? "download"                  --> doFloat
    , className =? "error"                     --> doFloat
    , className =? "notification"              --> doFloat
    , className =? "toolbar"                   --> doFloat
    , title     =? "Change Foreground Color"   --> doCenterFloat
    , title     =? "Change Background Color"   --> doCenterFloat
    , title     =? "Change color of selected text" --> doCenterFloat
    , resource  =? "desktop_window" --> doIgnore
    , isFullscreen             --> doFullFloat
    --                                      x y w h
    , manageDocks ] <+> manageHook def <+> namedScratchpadManageHook myScratchPads
    where
        doShiftAndGo ws = doF (W.greedyView ws) <+> doShift ws

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


myStartupHook = do
    -- return () >> checkKeymap myKeys
    spawn "/usr/bin/compton"
    spawn "xsetroot -cursor_name left_ptr"


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



mySort = getSortByXineramaRule

main :: IO ()
main = do
    dbus <- D.connectSession
    getWellKnownName dbus
    xmonad $   ewmhFullscreen . setEwmhWorkspaceSort mySort. ewmh   $ withUrgencyHook dzenUrgencyHook { args = [ "-bg", "orange", "-fg", "black"] }$ gnomeConfig {
                 terminal           = "gnome-terminal"
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
                 , manageHook         =  manageSpawn  <+> manageHook desktopConfig <+> myManageHook
                 , handleEventHook    =  handleEventHook desktopConfig
                 , logHook         =  do
                    dynamicLogWithPP (prettyPrinter dbus)
                    updatePointer (0.5, 0.5) (1, 1)
                    logHook desktopConfig
               }

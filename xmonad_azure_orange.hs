  -- Base
import XMonad
import System.Directory
import System.IO (hPutStrLn)
import System.Exit (exitSuccess)
import qualified XMonad.StackSet as W

    -- Actions
import XMonad.Actions.CopyWindow (kill1)
import XMonad.Actions.CycleWS (Direction1D(..), moveTo, shiftTo, doTo, WSType(..), nextScreen, prevScreen)
import XMonad.Actions.GridSelect
import qualified XMonad.Actions.TreeSelect as TS -- Tree Select Menu
import XMonad.Actions.MouseResize
import XMonad.Actions.Promote
import XMonad.Actions.RotSlaves (rotSlavesDown, rotAllDown)
-- import XMonad.Actions.WindowGo (runOrRaise)
import XMonad.Actions.WithAll (sinkAll, killAll)
--import qualified XMonad.Actions.Search as S

    -- Data
-- import Data.Char (isSpace, toUpper)
import Data.Maybe (fromJust)
import Data.Monoid
import Data.Maybe (isJust)
import Data.Tree -- Tree Select Menu
import qualified Data.Map as M

    -- Hooks
import XMonad.Hooks.DynamicLog (dynamicLogWithPP, wrap, xmobarPP, xmobarColor, shorten, PP(..))
import XMonad.Hooks.EwmhDesktops  -- for some fullscreen events, also for xcomposite in obs.
import XMonad.Hooks.ManageDocks (avoidStruts, docksEventHook, manageDocks, ToggleStruts(..), docks)
import XMonad.Hooks.ManageHelpers (isFullscreen, doFullFloat, doCenterFloat)
--import XMonad.Hooks.ServerMode
import XMonad.Hooks.SetWMName
import XMonad.Hooks.WorkspaceHistory -- Tree Select Menu
import XMonad.Layout.PerWorkspace
-- import XMonad.Hooks.StatusBar.PP.filterOutWsPP (scratchpadWorkspaceTag)
-- import XMonad.Hooks.StatusBar.PP (filterOutWsPP (scratchpadWorkspaceTag))

    -- Layouts
--import XMonad.Layout.Accordion
import XMonad.Layout.GridVariants (Grid(Grid))
import XMonad.Layout.SimplestFloat
import XMonad.Layout.Spiral
import XMonad.Layout.ResizableTile
import XMonad.Layout.Tabbed
--import XMonad.Layout.ThreeColumns

    -- Layouts modifiers
import XMonad.Layout.LayoutModifier
import XMonad.Layout.LimitWindows (limitWindows, increaseLimit, decreaseLimit)
import XMonad.Layout.Magnifier
import XMonad.Layout.MultiToggle (mkToggle, single, EOT(EOT), (??))
import XMonad.Layout.MultiToggle.Instances (StdTransformers(NBFULL, MIRROR, NOBORDERS))
import XMonad.Layout.NoBorders
import XMonad.Layout.Renamed
import XMonad.Layout.ShowWName
import XMonad.Layout.Simplest
import XMonad.Layout.Spacing
import XMonad.Layout.SubLayouts
import XMonad.Layout.WindowArranger (windowArrange, WindowArrangerMsg(..))
import XMonad.Layout.WindowNavigation
import qualified XMonad.Layout.ToggleLayouts as T (toggleLayouts, ToggleLayout(Toggle))
import qualified XMonad.Layout.MultiToggle as MT (Toggle(..))

   -- Utilities
import XMonad.Util.Dmenu
import XMonad.Util.EZConfig (additionalKeysP)
import XMonad.Util.NamedScratchpad
import XMonad.Util.Run (runProcessWithInput, safeSpawn, spawnPipe)
import XMonad.Util.SpawnOnce
import XMonad.Util.WorkspaceCompare (getSortByIndex) -- used in myShiftTo
-- import XMonad.Util.Hacks as Hacks

   -- ColorScheme module (SET ONLY ONE!)
      -- Possible choice are:
      -- DoomOne
      -- Dracula
      -- GruvboxDark
      -- MonokaiPro
      -- Nord
      -- OceanicNext
      -- Palenight
      -- SolarizedDark
      -- SolarizedLight
      -- TomorrowNight
import Colors.DoomOne

myFont :: String
--myFont = "xft:SauceCodePro Nerd Font Mono:regular:size=9:antialias=true:hinting=true"
myFont = "xft:Ubuntu:bold:size=11:antialias=true:hinting=true"

myModMask :: KeyMask
myModMask = mod4Mask        -- Sets modkey to windows key

myTerminal :: String
myTerminal = "kitty "    -- Sets default terminal

myScript :: String
myScript = "/home/azure/.config/shell/scripts/"

myFiles :: String
myFiles = "pcmanfm"
-- myFiles = "kitty -e fish -c lfub"

myCalendar :: String
myCalendar = "flatpak run re.sonny.Tangram"

myBrowser :: String
myBrowser = "brave "  -- Sets brave as browser

myMail :: String
myMail = "thunderbird "

myMusic :: String
myMusic = "spotify & ~/.config/shell/scripts/xmobar/spotify_title.sh"

myScreenshot :: String
myScreenshot = "flameshot screen -p ~/Pictures/screenshots"

myRegionScreenshot :: String
myRegionScreenshot = "flameshot gui -p ~/Pictures/screenshots"

myDelayedScreenshot :: String
myDelayedScreenshot = "flameshot screen -p ~/Pictures/screenshots -d 5000"

-- myEmacs :: String
-- myEmacs = "emacsclient -c -a 'emacs' "  -- Makes emacs keybindings easier to type

myEditor :: String
myEditor = "neovide "    -- Sets vim as editor
-- myEditor = myTerminal ++ " -e vim "    -- Sets vim as editor

myBorderWidth :: Dimension
myBorderWidth = 2           -- Sets border width for windows

myNormColor :: String       -- Border color of normal windows
myNormColor   = colorBack   -- This variable is imported from Colors.THEME

myFocusColor :: String      -- Border color of focused windows
myFocusColor  = color15     -- This variable is imported from Colors.THEME

-- Actions and Sounds

mySound :: String
mySound = "ffplay -nodisp -volume 50 -autoexit ~/.config/sounds/"

myStartupSound :: String
myStartupSound = mySound ++ "startup.wav"

myKillSound :: String
myKillSound = mySound ++ "azurian_plop.wav"

myShiftSound :: String
myShiftSound = mySound ++ "whoosh.wav"

myKill1 :: X ()
myKill1 = do ss <- gets windowset
             whenJust (W.peek ss) $ \w -> if W.member w $ delete'' w ss
                                        then windows $ delete'' w
                                        else kill <+> spawn myKillSound
    where delete'' w = W.modify Nothing (W.filter (/= w))

myShiftTo :: Direction1D -> WSType -> X ()
myShiftTo dir t = do ss <- gets windowset
                     whenJust (W.peek ss) $ \w -> if W.member w $ delete'' w ss
                                                then windows $ delete'' w
                                                else doTo dir t getSortByIndex (windows . W.shift) <+> spawn myShiftSound <+> doTo dir t getSortByIndex (windows . W.greedyView)
    where delete'' w = W.modify Nothing (W.filter (/= w))

-- END Actions and Sounds

windowCount :: X (Maybe String)
windowCount = gets $ Just . show . length . W.integrate' . W.stack . W.workspace . W.current . windowset

myStartupHook :: X ()
myStartupHook = do
    -- play startup sound
    spawnOnce myStartupSound
    -- spawn "setxkbmap -model acer_laptop -layout ch"

    spawn "killall trayer"  -- kill current trayer on each restart
    spawn ("sleep 2 && trayer --edge top --align right --widthtype request --padding 8 --margin 40 --distance 12 --iconspacing 5 --SetDockType true --SetPartialStrut true --expand true --monitor 1 --transparent true --alpha 60 --tint 0x202020 --height 22")--" ++ colorTrayer ++ " --height 22")

    -- spawnOnce "xmobar ~/.config/xmobar/xmobar.config"
    spawnOnce "xmobar ~/.config/xmobar/xmobarrc"

    spawnOnce "bash /home/azure/.config/shell/scripts/lock/locker.sh"
    spawnOnce "flameshot"
    -- spawn "killall compton && compton --config ~/.config/picom.conf"
    spawnOnce "picom"
    -- spawnOnce "picom --config ~/.config/picom.conf"

    --TRAYER applets
    spawnOnce "nm-applet --indicator" --netwoekmanager
    spawnOnce "blueman-applet" --bluetooth
    spawnOnce "udiskie --no-automount --tray" --disk mounting
    spawnOnce "birdtray" --thunderbird
    spawnOnce "sleep 5 && whatsapp-for-linux" --whatsapp
    
    -- NOTIFICATIONS
    -- spawnOnce "/usr/lib/notification-daemon/notification-daemon"
    -- spawnOnce "tiramisu"
    spawnOnce "dunst -conf $HOME/.config/dunst/dunstrc"
    -- spawnOnce "aw-qt"
    -- spawnOnce "pnmixer"
    spawnOnce "numlockx on"
    spawnOnce "systemctl --user enable batnotify.timer && systemctl --user start batnotify.timer"

    -- WALLPAPER
    -- spawnOnce "feh --randomize --bg-fill ~/.wallpapers/*"  -- feh set random wallpaper
    spawnOnce "/home/azure/.config/shell/scripts/background/background.sh"  -- feh set random wallpaper

    -- SYNCING
    spawnOnce "/home/azure/Applications/nextcloud.AppImage"
    spawnOnce "syncthing serve --no-browser"
    spawnOnce "a2ln 1997"
    
    
    setWMName "LG3D"

myColorizer :: Window -> Bool -> X (String, String)
myColorizer = colorRangeFromClassName
                  (0x28,0x2c,0x34) -- lowest inactive bg
                  (0x28,0x2c,0x34) -- highest inactive bg
                  (0xc7,0x92,0xea) -- active bg
                  (0xc0,0xa7,0x9a) -- inactive fg
                  (0x28,0x2c,0x34) -- active fg

-- treeSelect menu layout
myTreeConf = TS.TSConfig { TS.ts_hidechildren = True
                           , TS.ts_background   = 0xcc282c34
                           , TS.ts_font         = "xft:Ubuntu:bold"
                           , TS.ts_node         = (0xffc678dd, 0xff202328)
                           , TS.ts_nodealt      = (0xffc678dd, 0xff202020) 
                           , TS.ts_highlight    = (0xff000000, 0xff46D9FF) -- black, cyan
                           , TS.ts_extra        = 0xff94a187
                           , TS.ts_node_width   = 200
                           , TS.ts_node_height  = 30
                           , TS.ts_originX      = 0
                           , TS.ts_originY      = 250
                           , TS.ts_indent       = 80
                           , TS.ts_navigate     = TS.defaultNavigation
                           }

treeselectAction :: TS.TSConfig (X ()) -> X ()
treeselectAction myTreeConf = TS.treeselectAction myTreeConf
   [ Node (TS.TSNode "kitty"    "displays a kitten"           (spawn "kitty")) []
-- , Node (TS.TSNode "NAME" "DESCRIPTION" (return ()))
--     [ Node (TS.TSNode "" "" (spawn "")) []
--     , Node (TS.TSNode "" "" (spawn "")) []
--     ] Node (TS.TSNode "" "" (spawn "")) []
   , Node (TS.TSNode "files" "maximum power file manager" (spawn myFiles)) []
   , Node (TS.TSNode "accessories" "small utils for even smaller tasks" (return ()))
       [ Node (TS.TSNode "joplin" "notes and to-dos" (namedScratchpadAction myScratchPads "notes")) []
       , Node (TS.TSNode "calendar" "" (namedScratchpadAction myScratchPads "calendar")) []
       , Node (TS.TSNode "calculator" "" (namedScratchpadAction myScratchPads "calculator")) []
       , Node (TS.TSNode "neovim" "neovide" (spawn "neovide")) []
       ]
   , Node (TS.TSNode "internet" "your browser to choose" (return ()))
       [ Node (TS.TSNode "brave" "standard browser" (spawn "brave-browser")) []
       , Node (TS.TSNode "opera" "work browser" (spawn "opera")) []
       , Node (TS.TSNode "firefox" "media browser" (spawn "firefox")) []
       , Node (TS.TSNode "librewolf" "librewolf" (spawn "librewolf")) []
       , Node (TS.TSNode "badwolf" "badwolf" (spawn "badwolf")) []
       ]
   , Node (TS.TSNode "office" "writing documents and stuff" (return ()))
       [ Node (TS.TSNode "libreoffice" "for all your office needs" (spawn "flatpak run org.libreoffice.LibreOffice")) []
       , Node (TS.TSNode "vim" "the best text editor there is" (spawn (myTerminal ++ "-e vim"))) []
       , Node (TS.TSNode "neovim" "neovide" (spawn "neovide")) []
       , Node (TS.TSNode "onenote" "proprietary shit from Microsoft" (spawn "flatpak run re.sonny.Tangram")) []
       ]
   , Node (TS.TSNode "social" "chatting and more" (return ()))
       [ Node (TS.TSNode "mail" "the go to tool for writing messages" (spawn "thunderbird")) []
       , Node (TS.TSNode "teams" "proprietary microshit" (spawn "opera https://teams.microsoft.com &")) []
       , Node (TS.TSNode "whatsapp" "facebooks shitty messenger everybody uses" (spawn "flatpak run re.sonny.Tangram")) []
       ]
   , Node (TS.TSNode "graphics" "since 1822" (return ()))
       [ Node (TS.TSNode "gimp" "edit everything and more" (spawn "gimp")) []
       , Node (TS.TSNode "inkscape" "vector graphics and stuff" (spawn "inkscape")) []
       ]
   , Node (TS.TSNode "video" "it's bad for your health man ..." (return ()))
       [ Node (TS.TSNode "kdenlive" "video editing for free suckers" (spawn "kdenlive.AppImage")) []
       ]
   , Node (TS.TSNode "system" "system" (return ()))
       [ Node (TS.TSNode "brightness" "set screen brightness" (return ()))
           [ Node (TS.TSNode "bright" "FULL POWER!!"                    (spawn "brightnessctl s 100%")) []
           , Node (TS.TSNode "normal" "Is it half bright or half dark?" (spawn "brightnessctl s 50%"))  []
           , Node (TS.TSNode "dim"    "Pretty damn dark."               (spawn "brightnessctl s 1%"))  []
           ]
       , Node (TS.TSNode "power" "powermenu" (return ())) 
           [ Node (TS.TSNode "lock" "It's like gambling."         (spawn "~/.local/bin/lock")) []
           , Node (TS.TSNode "suspend" "That's the way to go."    (spawn "systemctl suspend && ~/.local/bin/lock")) []
           , Node (TS.TSNode "logout" "Why would you?"            (spawn "pkill xmonad")) []
           , Node (TS.TSNode "reboot" "See you later, alligator." (spawn "systemctl reboot")) []
           , Node (TS.TSNode "shutdown" "Bye, bye!"               (spawn "systemctl poweroff")) []
           ]
       ]
  ]

-- gridSelect menu layout
mygridConfig :: p -> GSConfig Window
-- mygridConfig colorizer = (buildDefaultGSConfig myColorizer)
mygridConfig colorizer = (buildDefaultGSConfig greenColorizer)
    { gs_cellheight   = 40
    , gs_cellwidth    = 200
    , gs_cellpadding  = 6
    , gs_originFractX = 0.5
    , gs_originFractY = 0.5
    , gs_font         = myFont
    }

-- | A green monochrome colorizer based on window class
greenColorizer = colorRangeFromClassName
                     black            -- lowest inactive bg
                     (0x70,0xFF,0x70) -- highest inactive bg
                     black            -- active bg
                     white            -- inactive fg
                     white            -- active fg
  where black = minBound
        white = maxBound

spawnSelectedSP :: [(String, String)] -> X ()
spawnSelectedSP lst = gridselect conf lst >>= flip whenJust (namedScratchpadAction myScratchPads) --spawn
    where conf = def
                   { gs_cellheight   = 40
                   , gs_cellwidth    = 200
                   , gs_cellpadding  = 6
                   , gs_originFractX = 0.5
                   , gs_originFractY = 0.5
                   , gs_font         = myFont
                   }

spawnSelected' :: [(String, String)] -> X ()
spawnSelected' lst = gridselect conf lst >>= flip whenJust spawn
    where conf = def
                   { gs_cellheight   = 40
                   , gs_cellwidth    = 200
                   , gs_cellpadding  = 6
                   , gs_originFractX = 0.5
                   , gs_originFractY = 0.5
                   , gs_font         = myFont
                   }

myAppGrid = 
  [ ("Terminal", "kitty")
  , ("Gimp", "gimp")
  , ("Writer", "libreoffice --writer")
  , ("Calc", "libreoffice --calc")
  , ("Impress", "libreoffice --impress")
  , ("Audacity", "audacity")
  , ("Spotify", "spotify")
  , ("OneNote", "onenote")
  ] <+> myBrowserGrid <+> myMessagingGrid

myBrowserGrid =
  [ ("qutebrowser", "qutebrowser")
  , ("badwolf", "badwolf")
  , ("firefox", "firefox")
  , ("opera", "opera")
  , ("brave", "brave-browser")
  , ("librewolf", "librewolf")
  ]

myMessagingGrid = 
  [ ("mail", (myMail))
  , ("whatsapp", "flatpak run io.github.mimbrero.WhatsAppDesktop")
  , ("signal", "flatpak run org.signal.Signal")
  , ("teams", "opera https://teams.microsoft.com &")
  , ("proton", "qutebrowser --qt-arg name ProtonMail --target window https://mail.proton.me/u/0/inbox")
  ]

{-
myScratchpadGrid = [ ("terminal", (namedScratchpadAction myScratchPads "terminal"))
                        , ("spotify", (namedScratchpadAction myScratchPads "spotify"))
                        , ("calculator", (namedScratchpadAction myScratchPads "calculator"))
                        , ("calendar", (namedScratchpadAction myScratchPads "calendar"))
                        , ("notes", (namedScratchpadAction myScratchPads "notes"))
                        , ("whatsapp", (namedScratchpadAction myScratchPads "whatsapp"))
                        , ("signal", (namedScratchpadAction myScratchPads "signal"))
                        , ("slack", (namedScratchpadAction myScratchPads "slack"))
                        ]
-}

myScratchPads :: [NamedScratchpad]
myScratchPads = 
  [ NS "terminal" spawnTerm findTerm manageTerm
  , NS "calendar" spawnCalendar findCalendar manageCalendar
  , NS "protonmail" spawnProtonMail findProtonMail manageProtonMail
  -- , NS "mocp" spawnMocp findMocp manageMocp
  , NS "calculator" spawnCalc findCalc manageCalc
  , NS "notes" spawnNotes findNotes manageNotes
  , NS "spotify" spawnSpot findSpot manageSpot
  , NS "slack" spawnSlack findSlack manageSlack
  , NS "whatsapp" spawnWA findWA manageWA
  , NS "signal" spawnSig findSig manageSig
  ]
  where
    spawnTerm  = myTerminal ++ " -T scratchpad"
    findTerm   = title =? "scratchpad"
    manageTerm = customFloating $ W.RationalRect l t w h
               where
                 h = 0.9
                 w = 0.9
                 t = 0.95 -h
                 l = 0.95 -w
    spawnCalendar  = myCalendar
    findCalendar   = className =? "Re.sonny.Tangram"
    manageCalendar = customFloating $ W.RationalRect l t w h
               where
                 h = 0.9
                 w = 0.7
                 t = 0.95 -h
                 l = 0.85 -w
    spawnProtonMail  = "qutebrowser --qt-arg name ProtonMail --target window https://mail.proton.me/u/0/inbox"
    findProtonMail   = className =? "ProtonMail"
    manageProtonMail = customFloating $ W.RationalRect l t w h
               where
                 h = 0.9
                 w = 0.7
                 t = 0.95 -h
                 l = 0.85 -w
    spawnNotes  = "joplin.AppImage"
    findNotes   = className =? "Joplin"
    manageNotes = customFloating $ W.RationalRect l t w h
               where
                 h = 0.9
                 w = 0.7
                 t = 0.95 -h
                 l = 0.85 -w
    spawnSpot  = myMusic
    findSpot   = className =? "Spotify"
    manageSpot = customFloating $ W.RationalRect l t w h
               where
                 h = 0.9
                 w = 0.9
                 t = 0.95 -h
                 l = 0.95 -w
    spawnMocp  = myTerminal ++ " -T mocp -e mocp"
    findMocp   = title =? "mocp"
    manageMocp = customFloating $ W.RationalRect l t w h
               where
                 h = 0.9
                 w = 0.9
                 t = 0.95 -h
                 l = 0.95 -w
    spawnCalc  = "qalculate"
    findCalc   = className =? "Qalculate"
    manageCalc = customFloating $ W.RationalRect l t w h
               where
                 h = 0.5
                 w = 0.4
                 t = 0.75 -h
                 l = 0.70 -w
    spawnSlack  = "slack"
    findSlack   = className =? "Slack"
    manageSlack = customFloating $ W.RationalRect l t w h
               where
                 h = 0.9
                 w = 0.7
                 t = 0.95 -h
                 l = 0.85 -w
    spawnWA  = "whatsapp-for-linux"
    findWA   = className =? "whatsapp-for-linux"
    manageWA = customFloating $ W.RationalRect l t w h
               where
                 h = 0.9
                 w = 0.7
                 t = 0.95 -h
                 l = 0.85 -w
    spawnSig  = "flatpak run org.signal.Signal"
    findSig   = className =? "Signal"
    manageSig = customFloating $ W.RationalRect l t w h
               where
                 h = 0.9
                 w = 0.7
                 t = 0.95 -h
                 l = 0.85 -w



--Makes setting the spacingRaw simpler to write. The spacingRaw module adds a configurable amount of space around windows.
mySpacing :: Integer -> l a -> XMonad.Layout.LayoutModifier.ModifiedLayout Spacing l a
mySpacing i = spacingRaw False (Border i i i i) True (Border i i i i) True

-- Below is a variation of the above except no borders are applied
-- if fewer than two windows. So a single window has no gaps.
mySpacing' :: Integer -> l a -> XMonad.Layout.LayoutModifier.ModifiedLayout Spacing l a
mySpacing' i = spacingRaw True (Border i i i i) True (Border i i i i) True


-- Defining a bunch of layouts, many that I don't use.
-- limitWindows n sets maximum number of windows displayed for layout.
-- mySpacing n sets the gap size around the windows.
tall     = renamed [Replace "tall"]
           $ smartBorders
           $ windowNavigation
           $ addTabs shrinkText myTabTheme
           $ subLayout [] (smartBorders Simplest)
           $ limitWindows 12
           $ mySpacing 8
           $ ResizableTall 1 (3/100) (1/2) []
--magnify  = renamed [Replace "magnify"]
--           $ smartBorders
--           $ windowNavigation
--           $ addTabs shrinkText myTabTheme
--           $ subLayout [] (smartBorders Simplest)
--           $ magnifier
--           $ limitWindows 12
--           $ mySpacing 8
--           $ ResizableTall 1 (3/100) (1/2) []
--monocle  = renamed [Replace "monocle"]
--           $ smartBorders
--           $ windowNavigation
--           $ addTabs shrinkText myTabTheme
--           $ subLayout [] (smartBorders Simplest)
--           $ limitWindows 20 Full
floats   = renamed [Replace "floats"]
           $ smartBorders
           $ limitWindows 20 simplestFloat
grid     = renamed [Replace "grid"]
           $ smartBorders
           $ windowNavigation
           $ addTabs shrinkText myTabTheme
           $ subLayout [] (smartBorders Simplest)
           $ limitWindows 12
           $ mySpacing 8
           $ mkToggle (single MIRROR)
           $ Grid (16/10)
spirals  = renamed [Replace "spirals"]
           $ smartBorders
           $ windowNavigation
           $ addTabs shrinkText myTabTheme
           $ subLayout [] (smartBorders Simplest)
           $ mySpacing' 8
           $ spiral (6/7)
--threeCol = renamed [Replace "threeCol"]
--           $ smartBorders
--           $ windowNavigation
--           $ addTabs shrinkText myTabTheme
--           $ subLayout [] (smartBorders Simplest)
--           $ limitWindows 7
--           $ ThreeCol 1 (3/100) (1/2)
--threeRow = renamed [Replace "threeRow"]
--           $ smartBorders
--           $ windowNavigation
--           $ addTabs shrinkText myTabTheme
--           $ subLayout [] (smartBorders Simplest)
--           $ limitWindows 7
           -- Mirror takes a layout and rotates it by 90 degrees.
           -- So we are applying Mirror to the ThreeCol layout.
--           $ Mirror
--           $ ThreeCol 1 (3/100) (1/2)
tabs     = renamed [Replace "tabs"]
           -- I cannot add spacing to this layout because it will
		   $ spacingRaw False (Border 10 0 10 10) True (Border 0 18 8 8) True
           -- add spacing between window and tabs which looks bad.
           $ tabbed shrinkText myTabTheme
--tallAccordion  = renamed [Replace "tallAccordion"]
--           $ Accordion
--wideAccordion  = renamed [Replace "wideAccordion"]
--           $ Mirror Accordion

-- setting colors for tabs layout and tabs sublayout.
myTabTheme = def { fontName            = myFont
                 , activeColor         = color15
                 , inactiveColor       = color08
                 , activeBorderColor   = color15
                 , inactiveBorderColor = colorBack
                 , activeTextColor     = colorBack
                 , inactiveTextColor   = color16
                 -- , mySpacing           = 150
                 }

-- Theme for showWName which prints current workspace when you change workspaces.
myShowWNameTheme :: SWNConfig
myShowWNameTheme = def
    { swn_font              = "xft:Ubuntu:bold:size=60"
    , swn_fade              = 1.0
    , swn_bgcolor           = "#1c1f24"
    , swn_color             = "#ffffff"
    }

-- The layout hook
myLayoutHook = showWName' myShowWNameTheme $ avoidStruts $ mouseResize $ windowArrange $ T.toggleLayouts floats
               $ mkToggle (NBFULL ?? NOBORDERS ?? EOT) myDefaultLayout
             where
               myDefaultLayout =     onWorkspaces [" www ", " chat "] myTABS myTALL
                                 ||| onWorkspaces [" www ", " chat "] grid myTABS
                                 ||| onWorkspaces [" www ", " chat "] spirals grid
                                 ||| onWorkspaces [" www ", " chat "] floats spirals
                                 ||| onWorkspaces [" www ", " chat "] myTALL floats
                               where 
                                 myTALL = withBorder myBorderWidth tall
                                 myTABS = noBorders tabs

--                                 ||| magnify
--                                 ||| noBorders monocle
--                                 ||| threeCol
--                                 ||| threeRow
--                                 ||| tallAccordion
--                                 ||| wideAccordion

-- myWorkspaces = [" 1 ", " 2 ", " 3 ", " 4 ", " 5 ", " 6 ", " 7 ", " 8 ", " 9 "]
-- myWorkspaces = [" dev ", " www ", " sys ", " doc ", " vbox ", " chat ", " mus ", " vid ", " gfx "]
myWorkspaces = [" cmd ", " www ", " sys ", " doc ", " gam ", " chat ", " med ", " gfx ", " vlt "]
myWorkspaceIndices = M.fromList $ zipWith (,) myWorkspaces [1..] -- (,) == \x y -> (x,y)

clickable ws = "<action=xdotool key super+"++show i++">"++ws++"</action>"
    where i = fromJust $ M.lookup ws myWorkspaceIndices

myManageHook :: XMonad.Query (Data.Monoid.Endo WindowSet)
myManageHook = composeAll
     -- 'doFloat' forces a window to float.  Useful for dialog boxes and such.
     -- using 'doShift ( myWorkspaces !! 7)' sends program to workspace 8!
     -- I'm doing it this way because otherwise I would have to write out the full
     -- name of my workspaces and the names would be very long if using clickable workspaces.
     [ className =? "confirm"         --> doFloat
     , className =? "file_progress"   --> doFloat
     , className =? "dialog"          --> doFloat
     , className =? "download"        --> doFloat
     , className =? "error"           --> doFloat
     , className =? "Gimp-2.10"            --> doFloat
     , className =? "notification"    --> doFloat
     , className =? "pinentry-gtk-2"  --> doFloat
     , className =? "splash"          --> doFloat
     , className =? "toolbar"         --> doFloat
     , className =? "Yad"             --> doCenterFloat
     , findCalendar  --> manageCalendar
     , title =? "Oracle VM VirtualBox Manager"  --> doFloat
     , className =? "Brave-browser"   --> doShift ( myWorkspaces !! 1 )
     , className =? "Opera"           --> doShift ( myWorkspaces !! 1 )
     , className =? "p3x-onenote"     --> doShift ( myWorkspaces !! 3 )
     , title =? "LibreOffice" --> doShift ( myWorkspaces !! 3 ) -- >>  moveTo ( myWorkspaces !! 3)
     , className =? "libreoffice" --> doShift ( myWorkspaces !! 3 )
     , className =? "libreoffice-startcenter" --> doShift ( myWorkspaces !! 3 )
     , className =? "libreoffice-writer" --> doShift ( myWorkspaces !! 3 )
     , className =? "libreoffice-calc" --> doShift ( myWorkspaces !! 3 )
     , className =? "libreoffice-impress" --> doShift ( myWorkspaces !! 3 )
     , className =? "libreoffice-math" --> doShift ( myWorkspaces !! 3 )
     --, className =? "VirtualBox Manager" --> doShift  ( myWorkspaces !! 4 )
     , className =? "thunderbird"     --> doShift ( myWorkspaces !! 5 )
     , className =? "Microsoft Teams - Preview" --> doShift ( myWorkspaces !! 5 )
     --, className =? "whatsapp-desktop-linux" --> doShift ( myWorkspaces !! 5 )
     --, className =? "Spotify"         --> doShift ( myWorkspaces !! 6 )
     , className =? "Firefox-esr"     --> doShift ( myWorkspaces !! 6 )
     , title =? "Mozilla Firefox"     --> doShift ( myWorkspaces !! 6 )
     , (className =? "mpv" <&&> title =? "video0 - mpv")             --> doFloat -- doShift ( myWorkspaces !! 6 )
     , className =? "Gimp-2.10"            --> doShift ( myWorkspaces !! 7 )
     , className =? "libreoffice-draw" --> doShift ( myWorkspaces !! 7 )
     , title =? "mpvFloating" --> doFloat
     , className =? "KeePassXC"       --> doShift ( myWorkspaces !! 8 )
     , (className =? "firefox" <&&> resource =? "Dialog") --> doFloat  -- Float Firefox Dialog
     --, isFullscreen -->  doFullFloat
     ] <+> (isFullscreen --> doFullFloat) <+> namedScratchpadManageHook myScratchPads
  where
    findCalendar   = className =? "Re.sonny.Tangram"
    manageCalendar = customFloating $ W.RationalRect l t w h
               where
                 h = 0.9
                 w = 0.7
                 t = 0.95 -h
                 l = 0.85 -w
 
-- START_KEYS
myKeyBindings :: [(String, X ())]
myKeyBindings =
    -- KB_GROUP Xmonad
        [ ("M-C-r", spawn "xmonad --recompile")       -- Recompiles xmonad
        , ("M-S-r", spawn "xmonad --restart")         -- Restarts xmonad
        -- , ("M-C-l", io exitSuccess)                   -- Quits xmonad

    -- KB_GROUP Get Help
        , ("M-<F1>", spawn "bash ~/.config/shell/scripts/help/xmonad_keys.sh") -- Get list of keybindings
        , ("M-<F2>", spawn "bash ~/.config/shell/scripts/help/vim_keys.sh")
        , ("M-<F3>", spawn "bash ~/.config/shell/scripts/help/lf_keys.sh")
        , ("M-<F4>", spawn "bash ~/.config/shell/scripts/help/curriculum.sh")
        -- , ("M-<F5>", spawn "dm-cast")
        , ("M-<F5>", spawn "directscreencast")
        , ("M-<F11>", sendMessage ToggleStruts)
        --, ("M-/", spawn "dtos-help")                  -- DTOS help/tutorial videos

    -- KB_GROUP Run Prompt
        , ("M-S-<Return>", spawn "dmenu_run -x 10 -y 13 -z 1900 -p \"Run: \"") -- Dmenu
        , ("M-r",      spawn "dmenu_run -x 10 -y 13 -z 1900 -p \"Run: \"")

    -- KB_GROUP Other Dmenu Prompts
    -- In Xmonad and many tiling window managers, M-p is the default keybinding to
    -- launch dmenu_run, so I've decided to use M-p plus KEY for these dmenu scripts.
        -- , ("M-p", spawn "dm-run")
        , ("M-p h", spawn "dm-hub")           -- allows access to all dmscripts
        , ("M-p s", spawn "dm-sounds")        -- choose an ambient background
        -- , ("M-p b", spawn "dm-setbg")         -- set a background
        -- , ("M-p c", spawn "dtos-colorscheme") -- choose a colorscheme
        , ("M-p c", spawn "dm-colpick")       -- pick color from our scheme
        -- , ("M-p e", spawn "dm-confedit")      -- edit config files
        -- , ("M-p i", spawn "dm-maim")          -- screenshots (images)
        , ("M-p k", spawn "dm-kill")          -- kill processes
        -- , ("M-p m", spawn "dm-man")           -- manpages
        , ("M-p n", spawn "dm-note")          -- store one-line notes and copy them
        -- , ("M-p o", spawn "dm-bookman")       -- qutebrowser bookmarks/history
        , ("M-p p", spawn "passmenu-otp -x 10 -y 13 -z 1900 -p \"Pass: \"") -- passmenu
        , ("M-p q", spawn "dm-logout")        -- logout menu
        , ("M-p r", spawn "dm-radio")         -- online radio
        , ("M-p y", spawn "dm-youtube")     -- search various search engines
        , ("M-p e", spawn "dm-emoji")     -- search various search engines
        -- , ("M-p s", spawn "dm-websearch")     -- search various search engines
        -- , ("M-p t", spawn "dm-translate")     -- translate text (Google Translate)

    -- KB_GROUP Useful programs to have a keybinding for launch
        , ("M-<Return>", spawn (myTerminal))

        -- Browsers
        , ("M-b b", spawn (myBrowser))
        , ("M-b o", spawn "opera")
        , ("M-b f", spawn "firefox")
        , ("M-b <Space>" , spawnSelected' myBrowserGrid)

        -- Chat
        , ("M-m m", spawn (myMail))
        , ("M-m p", spawn "qutebrowser --qt-arg name ProtonMail --target window https://mail.proton.me/u/0/inbox")
        , ("M-m t", spawn "opera https://teams.microsoft.com &")
        , ("M-m w", namedScratchpadAction myScratchPads "whatsapp")
        , ("M-m s", namedScratchpadAction myScratchPads "signal")
        , ("M-m <Space>" , spawnSelected' myMessagingGrid)
        -- , ("M-M1-w", spawn "whatsapp")

        -- Media
        -- , ("M-<Home>", spawn "spotify")
 
        -- file explorer
        , ("M-e e", spawn myFiles)
        , ("M-e t", spawn "thunar")
        
        , ("M-v", spawn "keepassxc")
        
        -- , ("M-C-w", spawn "feh --randomize --bg-fill ~/.wallpapers/*")
        , ("M-C-w", spawn "/home/azure/.config/shell/scripts/background/background.sh")
        , ("M-l", spawn "~/.local/bin/lock")
        , ("M-S-l", spawn "dm-logout")

    -- KB_GROUP Scratchpads
    -- Toggle show/hide these programs.  They run on a hidden workspace.
    -- When you toggle them to show, it brings them to your current workspace.
    -- Toggle them to hide and it sends them back to hidden workspace (NSP).
        , ("M-s t", namedScratchpadAction myScratchPads "terminal")
        , ("M-s s", namedScratchpadAction myScratchPads "spotify")
        , ("M-s c", namedScratchpadAction myScratchPads "calculator")
        , ("M-s p", namedScratchpadAction myScratchPads "calendar")
        , ("M-s n", namedScratchpadAction myScratchPads "notes")
        , ("M-s e", namedScratchpadAction myScratchPads "slack")
        , ("M-s w", namedScratchpadAction myScratchPads "whatsapp")
        , ("M-s q", namedScratchpadAction myScratchPads "signal")
        -- , ("M-s f", namedScratchpadAction myScratchPads "files")
        -- , ("M-s m", spawnSelected' myScratchpadGrid)

    -- KB_GROUP Kill windows
        , ("M-q" , myKill1)     -- Kill the currently focused client
        -- , ("M-S-c", kill1)     -- Kill the currently focused client
        -- , ("M-C-q", killAll)   -- Kill all windows on current workspace
        -- , ("M-S-a", killAll)   -- Kill all windows on current workspace

    -- KB_GROUP Tree Select (CTR-t followed by a key)
        , ("M-t", treeselectAction myTreeConf)

    -- KB_GROUP Grid Select (CTR-g followed by a key)
        , ("M-g g", spawnSelected' myAppGrid)                 -- grid select favorite apps
        , ("M-g t", goToSelected $ mygridConfig myColorizer)  -- goto selected window
        , ("M-g b", bringSelected $ mygridConfig myColorizer) -- bring selected window

    -- KB_GROUP Workspaces
        , ("M-.", nextScreen)  -- Switch focus to next monitor
        , ("M-,", prevScreen)  -- Switch focus to prev monitor
        , ("M-S-<Right>", myShiftTo Next nonNSP) -- Shifts focused window to next ws if there is a focused window
        , ("M-S-<Left>", myShiftTo Prev nonNSP) -- Shifts focused window to prev ws ''
        , ("M-<Left>", moveTo Prev nonNSP)
        , ("M-<Right>", moveTo Next nonNSP)

    -- KB_GROUP Floating windows
        , ("M-C-f", sendMessage (T.Toggle "floats")) -- Toggles my 'floats' layout
        , ("M-C-t", withFocused $ windows . W.sink)  -- Push floating window back to tile
        , ("M-S-t", sinkAll)                       -- Push ALL floating windows to tile

    -- KB_GROUP Windows navigation
        -- , ("M-m", windows W.focusMaster)  -- Move focus to the master window
        , ("M-j", windows W.focusDown)    -- Move focus to the next window
        , ("M-<Down>", windows W.focusDown)    -- Move focus to the next window
        , ("M-k", windows W.focusUp)      -- Move focus to the prev window
        , ("M-<Up>", windows W.focusUp)      -- Move focus to the prev window
        , ("M-S-m", windows W.swapMaster) -- Swap the focused window and the master window
        , ("M-S-j", windows W.swapDown)   -- Swap focused window with next window
        , ("M-S-k", windows W.swapUp)     -- Swap focused window with prev window
        , ("M-<Backspace>", promote)      -- Moves focused window to master, others maintain order
        , ("M-S-<Tab>", rotSlavesDown)    -- Rotate all windows except master and keep focus in place
        , ("M-C-<Tab>", rotAllDown)       -- Rotate all the windows in the current stack

    -- KB_GROUP Layouts
        , ("M-<Tab>", sendMessage NextLayout)           -- Switch to next layout
        , ("M-<Space>", sendMessage (MT.Toggle NBFULL) >> sendMessage ToggleStruts) -- Toggles noborder/full

    -- KB_GROUP Increase/decrease spacing (gaps)
        , ("C-M1-j", decWindowSpacing 4)         -- Decrease window spacing
        , ("C-M1-k", incWindowSpacing 4)         -- Increase window spacing
        , ("C-M1-h", decScreenSpacing 4)         -- Decrease screen spacing
        , ("C-M1-l", incScreenSpacing 4)         -- Increase screen spacing

    -- KB_GROUP Increase/decrease windows in the master pane or the stack
        , ("M-S-<Up>", sendMessage (IncMasterN 1))      -- Increase # of clients master pane
        , ("M-S-<Down>", sendMessage (IncMasterN (-1))) -- Decrease # of clients master pane
        , ("M-C-<Up>", increaseLimit)                   -- Increase # of windows
        , ("M-C-<Down>", decreaseLimit)                 -- Decrease # of windows

    -- KB_GROUP Window resizing
        , ("M-C-h", sendMessage Shrink)                   -- Shrink horiz window width
        , ("M-C-l", sendMessage Expand)                   -- Expand horiz window width
        , ("M-C-j", sendMessage MirrorShrink)          -- Shrink vert window width
        , ("M-C-k", sendMessage MirrorExpand)          -- Expand vert window width

    -- KB_GROUP Multimedia Keys
        , ("<XF86AudioPlay>", spawn "sp play")
        , ("<XF86AudioPrev>", spawn "sp prev")
        , ("<XF86AudioNext>", spawn "sp next")
        , ("<XF86AudioStop>", spawn "sp pause && sp prev")
        -- , ("<XF86AudioMute>", spawn "pactl set-sink-mute 0 mute")
        , ("<XF86AudioMute>", spawn "bash .config/shell/scripts/dunst/volume.sh mute")
        -- , ("<XF86AudioLowerVolume>", spawn "pactl -- set-sink-volume 0 -5%")
        , ("<XF86AudioLowerVolume>", spawn "bash .config/shell/scripts/dunst/volume.sh down")
        -- , ("<XF86AudioRaiseVolume>", spawn "pactl -- set-sink-volume 0 +5%")
        , ("<XF86AudioRaiseVolume>", spawn "bash .config/shell/scripts/dunst/volume.sh up")
        , ("<XF86HomePage>", spawn "brave-browser https://www.duckduckgo.com")
        -- , ("<XF86Search>", spawn "dm-websearch")
        , ("M-<KP_Divide>", spawn "dm-websearch")
        -- , ("<XF86Mail>", runOrRaise "thunderbird" (resource =? "thunderbird"))
        -- , ("<XF86Calculator>", runOrRaise "qalculate-gtk" (resource =? "qalculate-gtk"))
        -- , ("<XF86Eject>", spawn "toggleeject")
        , ("<XF86MonBrightnessUp>", spawn "brightnessctl s 5%+")
        , ("<XF86MonBrightnessDown>", spawn "brightnessctl s 5%-")
        --, ("<XF86PowerDown>", spawn "dm-logout")
        --, ("<XF86PowerOff>", spawn "dm-logout")
        , ("<Print>", spawn (myScreenshot))
        , ("M-S-<Print>", spawn (myRegionScreenshot))
        , ("M-<XF86XK_Launch1>", spawn (myRegionScreenshot))
        , ("M-M1-<Print>", spawn (myDelayedScreenshot))
        --, ("M-C-<Print>", spawn "dm-maim")
        ]
-- The following lines are needed for named scratchpads.
          where nonNSP          = WSIs (return (\ws -> W.tag ws /= "NSP"))
                nonEmptyNonNSP  = WSIs (return (\ws -> isJust (W.stack ws) && W.tag ws /= "NSP"))
    
-- Non-numeric num pad keys, sorted by number 
numPadKeys = [ "M-<KP_End>",  "M-<KP_Down>",  "M-<KP_Page_Down>" -- 1, 2, 3
             , "M-<KP_Left>", "M-<KP_Begin>", "M-<KP_Right>"     -- 4, 5, 6
             , "M-<KP_Home>", "M-<KP_Up>",    "M-<KP_Page_Up>"   -- 7, 8, 9
             , "M-<KP_Insert>"] -- 0

myKeys :: [(String, X ())]
myKeys = myKeyBindings
    {-[((k), windows $ f i)
        | (i, k) <- zip myWorkspaces numPadKeys
        , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]
    ]-}

	-- KB_GROUP Sublayouts
    -- This is used to push windows to tabbed sublayouts, or pull them out of it.
        -- , ("M-C-h", sendMessage $ pullGroup L)
        -- , ("M-C-l", sendMessage $ pullGroup R)
        -- , ("M-C-k", sendMessage $ pullGroup U)
        -- , ("M-C-j", sendMessage $ pullGroup D)
        -- , ("M-C-m", withFocused (sendMessage . MergeAll))
        -- -- , ("M-C-u", withFocused (sendMessage . UnMerge))
        -- , ("M-C-/", withFocused (sendMessage . UnMergeAll))
        -- , ("M-C-.", onGroup W.focusUp')    -- Switch focus to next tab
        -- , ("M-C-,", onGroup W.focusDown')  -- Switch focus to prev tab

    -- KB_GROUP Controls for mocp music player (SUPER-u followed by a key)
        -- , ("M-u p", spawn "mocp --play")
        -- , ("M-u l", spawn "mocp --next")
        -- , ("M-u h", spawn "mocp --previous")
        -- , ("M-u <Space>", spawn "mocp --toggle-pause")

    -- KB_GROUP Emacs (SUPER-e followed by a key)
        --, ("M-e e", spawn (myEmacs ++ ("--eval '(dashboard-refresh-buffer)'")))   -- emacs dashboard
        --, ("M-e b", spawn (myEmacs ++ ("--eval '(ibuffer)'")))   -- list buffers
        --, ("M-e d", spawn (myEmacs ++ ("--eval '(dired nil)'"))) -- dired
        --, ("M-e i", spawn (myEmacs ++ ("--eval '(erc)'")))       -- erc irc client
        --, ("M-e n", spawn (myEmacs ++ ("--eval '(elfeed)'")))    -- elfeed rss
        --, ("M-e s", spawn (myEmacs ++ ("--eval '(eshell)'")))    -- eshell
        --, ("M-e t", spawn (myEmacs ++ ("--eval '(mastodon)'")))  -- mastodon.el
        --, ("M-e v", spawn (myEmacs ++ ("--eval '(+vterm/here nil)'"))) -- vterm if on Doom Emacs
        --, ("M-e w", spawn (myEmacs ++ ("--eval '(doom/window-maximize-buffer(eww \"distro.tube\"))'"))) -- eww browser if on Doom Emacs
        --, ("M-e a", spawn (myEmacs ++ ("--eval '(emms)' --eval '(emms-play-directory-tree \"~/Music/\")'")))

-- END_KEYS

main :: IO ()
main = do
    -- Launching three instances of xmobar on their monitors.
    xmproc0 <- spawnPipe ("xmobar -x 0 ~/.config/xmobar/xmobarrc")
    xmproc1 <- spawnPipe ("xmobar -x 1 ~/.config/xmobar/xmobarrc")
    xmproc2 <- spawnPipe ("xmobar -x 2 ~/.config/xmobar/xmobarrc")
    -- the xmonad, ya know...what the WM is named after!
    xmonad $ ewmh def
        { manageHook         = myManageHook <+> manageDocks
        , handleEventHook    = docksEventHook
        -- , handleEventHook    = docks
                               -- Uncomment this line to enable fullscreen support on things like YouTube/Netflix.
                               -- This works perfect on SINGLE monitor systems. On multi-monitor systems,
                               -- it adds a border around the window if screen does not have focus. So, my solution
                               -- is to use a keybinding to toggle fullscreen noborders instead.  (M-<Space>)
                               -- <+> fullscreenEventHook
        , modMask            = myModMask
        , terminal           = myTerminal
        , startupHook        = myStartupHook
        , layoutHook         = myLayoutHook
        , workspaces         = myWorkspaces
        , borderWidth        = myBorderWidth
        , normalBorderColor  = myNormColor
        , focusedBorderColor = myFocusColor
        , logHook = dynamicLogWithPP $ namedScratchpadFilterOutWorkspacePP $ xmobarPP
        -- , logHook = dynamicLogWithPP $ scratchpadWorkspaceTag $ xmobarPP
              -- XMOBAR SETTINGS
              { ppOutput = \x -> hPutStrLn xmproc0 x   -- xmobar on monitor 1
                              >> hPutStrLn xmproc1 x   -- xmobar on monitor 2
                              >> hPutStrLn xmproc2 x   -- xmobar on monitor 3
                -- Current workspace
              , ppCurrent = xmobarColor color06 "" . wrap
                            -- ("<box type=Bottom width=2 mb=2 color=" ++ color06 ++ ">") "</box>"
                            ("[") "]"
                -- Visible but not current workspace
              , ppVisible = xmobarColor color06 "" . clickable
                -- Hidden workspace
              , ppHidden = xmobarColor color12 "" . wrap
                           ("<box type=Top width=2 mt=2 color=" ++ color12 ++ ">") "</box>" . clickable
                -- Hidden workspaces (no windows)
              , ppHiddenNoWindows = xmobarColor color12 ""  . clickable
                -- Title of active window
              , ppTitle = xmobarColor color16 "" . shorten 60
                -- Separator character
              , ppSep =  "<fc=" ++ color09 ++ "> <fn=1>|</fn> </fc>"
                -- Urgent workspace
              , ppUrgent = xmobarColor color02 "" . wrap "!" "!"
                -- Adding # of windows on current workspace to the bar
              , ppExtras  = [windowCount]
                -- order of things in xmobar
              , ppOrder  = \(ws:l:t:ex) -> [ws,l]++ex++[t]
              }
        } `additionalKeysP` myKeys -- ++ mynumkeys

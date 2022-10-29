{-# OPTIONS_GHC -W -fwarn-unused-imports -fno-warn-missing-signatures -O2 #-}
import Control.Monad (liftM2)
import Data.Map (fromList, toList, union)
import System.Environment (getEnv, setEnv)
import System.IO (hPutStrLn)
import XMonad
import XMonad.Actions.CopyWindow (copy, kill1, killAllOtherCopies)
import XMonad.Actions.CycleWS (moveTo, nextWS, emptyWS, WSType(Not)
                              , prevWS, shiftToNext, shiftToPrev, toggleWS)
import XMonad.Actions.DynamicWorkspaces (addWorkspacePrompt, removeWorkspace
                                        , renameWorkspace, selectWorkspace
                                        , withWorkspace)
import XMonad.Actions.GridSelect (HasColorizer, GSConfig, buildDefaultGSConfig
                                 , defaultColorizer, goToSelected
                                 , gridselectWorkspace, gs_navigate, navNSearch)
import XMonad.Actions.Submap (submap)
import XMonad.Hooks.DynamicLog (dzenColor, dynamicLogWithPP
                               , ppCurrent, ppVisible, ppSep, ppHidden
                               , ppUrgent, ppTitle, ppExtras
                               , ppOrder, ppOutput, trim, wrap)
import XMonad.Hooks.ManageDocks (avoidStruts, docks, docksEventHook
                                , ToggleStruts(ToggleStruts))
import XMonad.Hooks.ManageHelpers (isFullscreen, doFullFloat)
import XMonad.Hooks.UrgencyHook (focusUrgent)
import XMonad.Layout.Accordion (Accordion(Accordion))
import XMonad.Layout.Dishes (Dishes(Dishes))
import XMonad.Layout.FixedColumn (FixedColumn(FixedColumn))
import XMonad.Layout.Grid (Grid(Grid))
import XMonad.Layout.LimitWindows (limitWindows)
import XMonad.Layout.Magnifier (magnifiercz')
import XMonad.Layout.MultiToggle (mkToggle, single, Toggle(Toggle))
import XMonad.Layout.MultiToggle.Instances (StdTransformers(MIRROR))
import XMonad.Layout.NoBorders (smartBorders)
import XMonad.Layout.NoFrillsDecoration (activeBorderColor, activeColor
                                        , activeTextColor, decoHeight, fontName
                                        , inactiveBorderColor, inactiveColor
                                        , inactiveTextColor, urgentColor
                                        , urgentTextColor)
import XMonad.Layout.PerWorkspace (onWorkspace)
import XMonad.Layout.ResizableTile (ResizableTall(ResizableTall))
import XMonad.Layout.Tabbed (shrinkText, tabbed)
import XMonad.Layout.ThreeColumns (ThreeCol(ThreeColMid))
import XMonad.Layout.ToggleLayouts (toggleLayouts, ToggleLayout(ToggleLayout))
import XMonad.Layout.WindowNavigation (Navigate(Move))
import XMonad.Prompt (fgColor, bgColor, XPPosition(Bottom), height, font
                     , position, promptBorderWidth)
import XMonad.Prompt.Shell (shellPrompt)
import XMonad.Util.Loggers (date, logCmd)
import XMonad.Util.Run (spawnPipe)
import XMonad.Util.Themes (theme, themeAuthor, themeDescription, ThemeInfo(TI)
                          , themeName)
import XMonad.Util.Types (Direction1D(Next, Prev), Direction2D(L, R, U, D))
import XMonad.StackSet (shiftMaster, greedyView, shift)

---------------
--  BINDINGS
---------------

myMod = mod4Mask -- windows key
myCtrl = controlMask
myTerminal = "urxvtc"

-- Mouse bindings: default actions bound to mouse events
myMouseBindings (XConfig {XMonad.modMask = modMask}) = fromList $
    -- mod-button1 %! Set the window to floating mode and move by dragging
    [ ((modMask, button1), (\w -> focus w >> mouseMoveWindow w
                                          >> windows shiftMaster))
    -- mod-button2 %! Raise the window to the top of the stack
    , ((modMask, button2), (\w -> focus w >> windows shiftMaster))
    -- mod-button3 %! Set the window to floating mode and resize by dragging
    , ((modMask, button3), (\w -> focus w >> mouseResizeWindow w
                                          >> windows shiftMaster))
    -- you may also bind events to the mouse scroll wheel (button4 and button5)
    , ((modMask, button5), (\_ -> moveTo Next (Not emptyWS)))
    , ((modMask, button4), (\_ -> moveTo Prev (Not emptyWS)))
    -- scroll wheel click, bottom right corner on trackball
    , ((modMask, 6), (\w -> focus w >> kill))
    , ((modMask, 8), (\w -> focus w >> kill))
    ]

mykeys _ = [
  ((myMod, xK_Left), prevWS)
  , ((myMod, xK_Right), nextWS)
  , ((myMod .|. shiftMask, xK_Left),  shiftToPrev >> prevWS)
  , ((myMod .|. shiftMask, xK_Right), shiftToNext >> nextWS)
  , ((myMod, xK_a), toggleWS)
  , ((myMod, xK_z), shellPrompt myXPConfig)
  , ((myMod, xK_g), goToSelected myGSConfig)
  , ((myMod .|. shiftMask, xK_m), sendMessage $ Toggle MIRROR)
  , ((myMod .|. shiftMask, xK_g), gridselectWorkspace myGSConfig greedyView)
  , ((myMod, xK_x), sendMessage ToggleStruts)
  , ((myMod, xK_F1), spawn "${HOME}/.screenlayout/`hostname`-work.sh")
  , ((myMod, xK_F2), spawn "${HOME}/.screenlayout/`hostname`-home.sh")
  , ((myMod, xK_F3), spawn "${HOME}/.screenlayout/`hostname`-solo.sh")
  , ((myMod, xK_F4), spawn "sleep 0.5 && xset dpms force suspend")
  , ((myMod, xK_F5), spawn "sleep 0.5 && xset dpms force off")
  , ((myMod, xK_F6), spawn "sleep 0.5 && ${HOME}/bin/icd && ${HOME}/bin/mice.sh")
  , ((myMod, xK_Up),   spawn "sleep 0.5 && b u")
  , ((myMod, xK_Down), spawn "sleep 0.5 && b d")
  , ((myMod, xK_l), spawn "slock xset dpms force off")
  , ((myMod .|. myCtrl .|. shiftMask, xK_Right), sendMessage $ Move R)
  , ((myMod .|. myCtrl .|. shiftMask, xK_Left),  sendMessage $ Move L)
  , ((myMod .|. myCtrl .|. shiftMask, xK_Up),    sendMessage $ Move U)
  , ((myMod .|. myCtrl .|. shiftMask, xK_Down),  sendMessage $ Move D)
  , ((myMod, xK_BackSpace), focusUrgent)
  , ((myMod, xK_s), sendMessage $ ToggleLayout)
  , ((myMod, xK_b), submap . fromList $
                    [ ((m, k), f)
                    | m <- [0, myMod]
                    , (k, f) <- [ (xK_a, addWorkspacePrompt myXPConfig)
                                , (xK_c, withWorkspace myXPConfig
                                         (windows . copy))
                                , (xK_d, kill1)
                                , (xK_k, removeWorkspace)
                                , (xK_m, withWorkspace myXPConfig
                                         (windows . shift))
                                , (xK_o, killAllOtherCopies)
                                , (xK_r, renameWorkspace myXPConfig)
                                , (xK_s, selectWorkspace myXPConfig)
                                ]
                    ]
    )]

dvorakify kl = fromList $ map (\((m, k), d) -> ((m, dk k), d)) $ toList kl
  where
    dk :: KeySym -> KeySym
    dk k    | k == xK_grave = xK_dollar
            | k == xK_asciitilde = xK_asciitilde

            | k == xK_1 = xK_ampersand
            | k == xK_exclam = xK_percent

            | k == xK_2 = xK_bracketleft
            | k == xK_at = xK_7

            | k == xK_3 = xK_braceleft
            | k == xK_numbersign = xK_5

            | k == xK_4 = xK_braceright
            | k == xK_dollar = xK_3

            | k == xK_5 = xK_parenleft
            | k == xK_percent = xK_1

            | k == xK_6 = xK_equal
            | k == xK_asciicircum = xK_9

            | k == xK_7 = xK_asterisk
            | k == xK_ampersand = xK_0

            | k == xK_8 = xK_parenright
            | k == xK_asterisk = xK_2

            | k == xK_9 = xK_plus
            | k == xK_parenleft = xK_4

            | k == xK_0 = xK_bracketright
            | k == xK_parenright = xK_6

            | k == xK_minus = xK_exclam
            | k == xK_underscore = xK_8

            | k == xK_equal = xK_numbersign
            | k == xK_plus = xK_grave

            | k == xK_q = xK_semicolon -- upper row, left side
            | k == xK_Q = xK_colon

            | k == xK_w = xK_comma
            | k == xK_W = xK_less

            | k == xK_e = xK_period
            | k == xK_E = xK_greater

            | k == xK_bracketleft = xK_slash -- upper row, top right
            | k == xK_braceleft = xK_question

            | k == xK_bracketright = xK_at
            | k == xK_braceright = xK_asciicircum

            | k == xK_R = xK_P
            | k == xK_T = xK_Y
            | k == xK_Y = xK_F
            | k == xK_U = xK_G
            | k == xK_I = xK_C
            | k == xK_O = xK_R
            | k == xK_P = xK_L
            | k == xK_A = xK_A
            | k == xK_S = xK_O
            | k == xK_D = xK_E
            | k == xK_F = xK_U
            | k == xK_G = xK_I
            | k == xK_H = xK_D
            | k == xK_J = xK_H
            | k == xK_K = xK_T
            | k == xK_L = xK_N
            | k == xK_Z = xK_quotedbl
            | k == xK_X = xK_Q
            | k == xK_C = xK_J
            | k == xK_V = xK_K
            | k == xK_B = xK_X
            | k == xK_N = xK_B
            | k == xK_M = xK_M

            | k == xK_r = xK_p
            | k == xK_t = xK_y
            | k == xK_y = xK_f
            | k == xK_u = xK_g
            | k == xK_i = xK_c
            | k == xK_o = xK_r
            | k == xK_p = xK_l
            | k == xK_a = xK_a
            | k == xK_s = xK_o
            | k == xK_d = xK_e
            | k == xK_f = xK_u
            | k == xK_g = xK_i
            | k == xK_h = xK_d
            | k == xK_j = xK_h
            | k == xK_k = xK_t
            | k == xK_l = xK_n
            | k == xK_z = xK_apostrophe
            | k == xK_x = xK_q
            | k == xK_c = xK_j
            | k == xK_v = xK_k
            | k == xK_b = xK_x
            | k == xK_n = xK_b
            | k == xK_m = xK_m

            | k == xK_comma = xK_w -- bottom right
            | k == xK_less = xK_W

            | k == xK_period = xK_v
            | k == xK_greater = xK_V

            | k == xK_slash = xK_z
            | k == xK_question = xK_Z

            | otherwise = k

-------------
-- THEMES
-------------

-- Color names are easier to remember:
colorBlack           = "#000000"
colorOrange          = "#ff7701"
-- colorDarkGray        = "#171717"
-- colorPink            = "#e3008d"
colorGreen           = "#00aa4a"
colorBlue            = "#008dd5"
colorYellow          = "#fee100"
colorWhite           = "#cfbfad"
-- colorBrightGreen     = "#00FF00"
colorRed             = "#FF0000"

xftFont = "Inconsolata:size=8"

newTheme :: ThemeInfo
newTheme = TI "" "" "" def

rakTheme :: ThemeInfo
rakTheme =
    newTheme { themeName        = "rakTheme"
             , themeAuthor      = "Ryan Kavanagh"
             , themeDescription = "Small decorations: orange and blue theme"
             , theme            = def { activeColor         = colorBlack
                                      , inactiveColor       = colorBlack
                                      , activeBorderColor   = colorOrange
                                      , inactiveBorderColor = colorBlack
                                      , activeTextColor     = colorOrange
                                      , inactiveTextColor   = colorBlue
                                      , urgentColor         = colorRed
                                      , urgentTextColor     = colorYellow
                                      , decoHeight          = 13
                                      , fontName            = "Inconsolata"
                                      }
             }

myTheme = theme rakTheme

myXPConfig = def {
    fgColor  = "white"
  , bgColor  = "black"
  , promptBorderWidth = 0
  , position = Bottom
  , height   = 15
  , font = "xft:" ++ xftFont
  }

-----------------
-- LAYOUTS
-----------------

-- My workspaces

myWorkspaces = ["term", "web", "chatter", "reading", "gnus", "music", "LaTeX", "work", "radio"]
               ++ (map show [9..20])

-- smartBorders removes borders when there's no ambiguity
myLayout = mkToggle (single MIRROR) (smartBorders $ toggleLayouts Full
                                     $ avoidStruts perWS)
  where
    -- Per workspace layout selection.
    perWS = onWorkspace "term"  (myTall ||| customRyan) $
            onWorkspace "web"   (tabbed shrinkText myTheme
                                 ||| mySplit ||| myTCM) $
            onWorkspace "LaTeX" latexFirst $
            customRyan

    -- Each of these allows toggling through a set of layouts
    -- in the same logical order, but from a different starting point.
    customRyan = myGrid  ||| myDish  ||| Accordion ||| myCode
                 ||| myFixed ||| myTall
    latexFirst = myFixed ||| customRyan

    -- This is a three column mode, with the master in the middle.
    myTCM = ThreeColMid 1 (3/100) (1/2)
    -- This is a tall-like layout with magnification.
    -- The master window is fixed at 80 columns wide, making this good
    -- for coding. Limited to 3 visible windows at a time to ensure all
    -- are a good size.
    myCode = limitWindows 3 $ magnifiercz' 1.4 $ FixedColumn 1 20 80 10

    -- Stack with one large master window.
    -- It's easy to overflow a stack to the point that windows are too
    -- small, so only show first 5.
    myDish = limitWindows 5 $ Dishes nmaster ratio
      where
        -- The default number of windows in the master pane
        nmaster = 1
        -- Default proportion of screen occupied by other panes
        ratio = 1/5

    -- Split screen, optimized for web browsing.
    mySplit = magnifiercz' 1.4 $ Tall nmaster delta ratio
      where
        -- The default number of windows in the master pane
        nmaster = 1
        -- Percent of screen to increment by when resizing panes
        delta   = 3/100
        -- Default proportion of screen occupied by master pane
        ratio   = 60/100

    -- Standard grid.
    myGrid = Grid

    myFixed = FixedColumn 1 20 80 10

    myTall = ResizableTall nmaster delta ratio [50/100]
        where
            nmaster = 1
            delta = 3/100
            ratio = 1/2

-------------------
-- STATUS BAR
-------------------

statusBarCmd = "dzen2" ++
               " -dock" ++
               " -bg '" ++ colorBlack ++ "'" ++
               " -fg '" ++ colorBlue ++ "'" ++
               " -sa c" ++
               " -fn '" ++ xftFont ++ "'" ++
               " -w 925 -x 0 -y 0 -ta l -e ''"

-- dynamiclog pretty printer for dzen
mPP h = def
        { ppCurrent = dzenColor colorOrange colorBlack
        , ppVisible = dzenColor colorGreen colorBlack
        , ppHidden  = dzenColor colorBlue colorBlack
        , ppUrgent  = dzenColor colorRed colorBlack . wrap "[" "]"
        , ppSep     = dzenColor "grey60" colorBlack " ^r(1x8) "
        , ppTitle   = dzenColor colorWhite colorBlack . trim
        , ppOrder   = \(ws:l:t:d:b:o) -> [b,d,ws,l,t] ++ o
        , ppExtras  = [date "%H:%M:%S", batteryCheck]
        , ppOutput  = hPutStrLn h
        }
    where
        batteryCheck = logCmd ("${HOME}/.dzen/battery.sh")

---------------------
-- GENERAL CONFIG
--------------------

myGSConfig :: HasColorizer a => GSConfig a
myGSConfig = (buildDefaultGSConfig defaultColorizer) { gs_navigate = navNSearch }

myManageHook = composeAll . concat $
                [ [ className =? c --> doFloat             | c <- floats]
                , [ className =? c --> viewShift "web"     | c <- web]
                , [ className =? c --> viewShift "chatter" | c <- chatter]
                , [ className =? c --> viewShift "music"   | c <- music]
                , [ isFullscreen   --> doFullFloat ]
              ]
  where
    viewShift = doF . liftM2 (.) greedyView shift
    floats    = ["MPlayer"]
    web       = ["Firefox", "Firefox-esr", "Navigator", "google-chrome", "Google-chrome"]
    chatter   = ["slack", "Slack"]
    music     = ["pavucontrol", "Pavucontrol"]

myConfig dzenPipe = docks $ def {
      workspaces           = myWorkspaces
      , modMask            = myMod
      , terminal           = myTerminal
      , normalBorderColor  = inactiveBorderColor myTheme
      , focusedBorderColor = activeBorderColor myTheme
      , borderWidth        = 1
      , layoutHook         = myLayout
      , manageHook         = manageHook def <+> myManageHook
      , mouseBindings      = myMouseBindings
      , keys               = \x -> (fromList $ mykeys x)
                                   `union` dvorakify (keys def x)
      , logHook            = dynamicLogWithPP $ mPP dzenPipe
      }

main = do
    dzenPipe <- spawnPipe statusBarCmd
    path <- getEnv "PATH"
    home <- getEnv "HOME"
    setEnv "PATH" (home ++ "/bin:" ++ path)
    xmonad $ myConfig dzenPipe

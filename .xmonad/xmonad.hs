{-# OPTIONS_GHC -W -fwarn-unused-imports -fno-warn-missing-signatures -O2 #-}
import XMonad
-- import XMonad.Config.Kde
import XMonad.Actions.CopyWindow
import XMonad.Actions.CycleWS
import XMonad.Actions.DynamicWorkspaces
import XMonad.Actions.CopyWindow(copy)
import XMonad.Actions.GridSelect
import XMonad.Actions.PerWorkspaceKeys
import XMonad.Actions.RotSlaves
import XMonad.Actions.Submap
import XMonad.Actions.UpdatePointer
import XMonad.Config.Desktop
-- import XMonad.Config.Kde
import XMonad.Hooks.DynamicLog hiding (xmobar, xmobarPP, xmobarColor, sjanssenPP, byorgeyPP)
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageDocks (avoidStruts)
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.UrgencyHook
import XMonad.Layout.Accordion
import XMonad.Layout.Combo
import XMonad.Layout.Cross
import XMonad.Layout.Dishes
import XMonad.Layout.FixedColumn
import XMonad.Layout.Grid
import XMonad.Layout.IM
import XMonad.Layout.LimitWindows
import XMonad.Layout.Magnifier
import XMonad.Layout.NoBorders
import XMonad.Layout.NoFrillsDecoration
import XMonad.Layout.PerWorkspace
import XMonad.Layout.PerWorkspace
import XMonad.Layout.Reflect
import XMonad.Layout.ResizableTile
import XMonad.Layout.SimpleDecoration
import XMonad.Layout.Tabbed (tabbed)
import XMonad.Layout.ThreeColumns
import XMonad.Layout.ToggleLayouts
import XMonad.Layout.TwoPane
import XMonad.Layout.WindowNavigation
import XMonad.Prompt
import XMonad.Prompt.Shell
import XMonad.Util.EZConfig (additionalKeys)
import XMonad.Util.Loggers
import XMonad.Util.NamedWindows
import XMonad.Util.Run -- for spawnPipe
import XMonad.Util.Themes
import qualified XMonad.StackSet as W
import qualified Data.Map as M
import Control.Monad
import Data.Ratio ((%))
import Data.Maybe
import Data.List
import System.IO (hPutStrLn)

myMod = mod4Mask -- windows key
myCtrl = controlMask
myTerminal = "urxvtc"

-- My workspaces

myWorkspaces = ["term", "web", "chatter", "code", "K/D", "music", "plasma", "LaTeX", "web-nb"] ++ (map show [10..20])

-- Mouse bindings: default actions bound to mouse events
myMouseBindings (XConfig {XMonad.modMask = modMask}) = M.fromList $

    -- mod-button1 %! Set the window to floating mode and move by dragging
    [ ((modMask, button1), (\w -> focus w >> mouseMoveWindow w
                                          >> windows W.shiftMaster))
    -- mod-button2 %! Raise the window to the top of the stack
    , ((modMask, button2), (\w -> focus w >> windows W.shiftMaster))
    -- mod-button3 %! Set the window to floating mode and resize by dragging
    , ((modMask, button3), (\w -> focus w >> mouseResizeWindow w
                                          >> windows W.shiftMaster))
    -- you may also bind events to the mouse scroll wheel (button4 and button5)
    , ((modMask, button5), (\_ -> moveTo Next NonEmptyWS))
    , ((modMask, button4), (\_ -> moveTo Prev NonEmptyWS ))

    , ((modMask .|. shiftMask, button5), (\w -> focus w >> kill ))
    ]

-- Theme {{{
-- Color names are easier to remember:
colorBlack           = "#000000"
colorOrange          = "#ff7701"
colorDarkGray        = "#171717"
colorPink            = "#e3008d"
colorGreen           = "#00aa4a"
colorBlue            = "#008dd5"
colorYellow          = "#fee100"
colorWhite           = "#cfbfad"
colorBrightGreen     = "#00FF00"
colorRed             = "#FF0000"

colorNormalBorder    = "#1c2636"
colorFocusedBorder   = "#2797d8"
barFont = "'-misc-fixed-medium-r-semicondensed--12-110-75-75-c-60-iso8859-1'"
barXFont = barFont
xftFont = "xft: inconsolata-14"
--}}}
statusBarCmd = "dzen2" ++
               " -bg '" ++ colorBlack ++ "'" ++
               " -fg '" ++ colorBlue ++ "'" ++
               " -sa c" ++
               " -fn '" ++ barXFont ++ "'" ++
               " -w 808 -x 114 -y 0 -ta l -e ''"

newTheme :: ThemeInfo
newTheme = TI "" "" "" defaultTheme

rakTheme :: ThemeInfo
rakTheme =
    newTheme { themeName        = "rakTheme"
             , themeAuthor      = "Ryan Kavanagh"
             , themeDescription = "Small decorations: orange and blue theme"
             , theme            = defaultTheme { activeColor         = colorBlack
                                               , inactiveColor       = colorBlack
                                               , activeBorderColor   = colorOrange
                                               , inactiveBorderColor = colorBlue
                                               , activeTextColor     = colorOrange
                                               , inactiveTextColor   = colorBlue
                                               , urgentColor         = colorRed
                                               , urgentTextColor     = colorYellow
                                               , decoHeight          = 13
                                               , fontName            = "xft: inconsolata-8:pixelsize=10:weight=bold"
                                               }
             }

myTheme = theme rakTheme

myXPConfig = defaultXPConfig {
    fgColor  = "white"
  , bgColor  = "black"
  , promptBorderWidth = 0
  , position = Bottom
  , height   = 15
  }


myLayout = smartBorders $ toggleLayouts Full perWS
  where
    -- Per workspace layout selection.
    perWS = onWorkspace "web"     (noTitles   $ (tabbed shrinkText myTheme ||| myTCM ||| mySplit ||| myWide)) $
            onWorkspace "term"    (noTitles   $ (Full ||| myTall2 ||| customRyan)) $
            onWorkspace "chatter" (noTitles   $ myChat gridFirst) $
            onWorkspace "code"    (noTitles   $ codeFirst) $
            onWorkspace "LaTeX"   (noTitles   $ latexFirst) $
                                  (noTitles $ customRyan)

    -- Modifies a layout to be desktop friendly with title bars
    -- and avoid the panel.
    withTitles l = noFrillsDeco shrinkText myTheme (desktopLayoutModifiers l)
    -- Modifies a layout to be desktop friendly, but with no title bars
    -- and avoid the panel.
    noTitles l = desktopLayoutModifiers l

    -- Each of these allows toggling through a set of layouts
    -- in the same logical order, but from a different starting point.
    codeFirst  = myCode  ||| myTCM   ||| myWide  ||| myGrid   ||| myDish
    gridFirst  = myGrid  ||| myDish  ||| myCode  ||| myWide
    customRyan = myGrid  ||| myDish  ||| Accordion ||| myCode ||| myWide ||| simpleCross ||| myFixed ||| myTall
    latexFirst = myLaTeX ||| myFixed ||| myGrid  ||| myTall

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

    -- Wide layout with subwindows at the bottom.
    myWide = Mirror $ Tall nmaster delta ratio
      where
        -- The default number of windows in the master pane
        nmaster = 1
        -- Percent of screen to increment by when resizing panes
        delta   = 3/100
        -- Default proportion of screen occupied by master pane
        ratio   = 80/100

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

    -- The chat workspace has a roster on the right.
    myChat base = reflectHoriz $ withIM (1%5) ((And (ClassName "Pidgin") (Role "buddy_list")) `Or` (And (Title "Kopete") (Role "MainWindow#1"))) base
      where
        role = stringProperty "WM_WINDOW_ROLE"
        command = stringProperty "WM_COMMAND"

    -- LaTeX layout with a XPDF, vim window and small console to run pdflatex
    -- From
    -- http://www.haskell.org/haskellwiki/Xmonad/Config_archive/iderrick_xmonad.hs
    myLaTeX = windowNavigation (
                combineTwo
                (TwoPane delta 0.45)
                (Full)
                (combineTwo
                 (Mirror (TwoPane delta 0.85))
                 (Full)
                 (Full)
                )
            )
        where
            -- Percent of the screen to increment by when resizing panes
            delta = 3/100

    myTall = ResizableTall nmaster delta ratio [50/100]
        where
            nmaster = 1
            delta = 3/100
            ratio = 55/100

    myTall2 = ResizableTall nmaster delta ratio [50/100]
        where
            nmaster = 1
            delta = 3/100
            ratio = 1/2


-- Pretty printer {{{
-- dynamiclog pretty printer for dzen
mPP h = defaultPP
        { ppCurrent = wrap ("^fg(" ++ colorOrange ++ ")^bg(" ++ colorBlack ++ ")^p(2)") "^p(2)^fg()^bg()"
        , ppVisible = wrap ("^fg(" ++ colorBlue ++ ")^bg(" ++ colorBlack ++ ")^p(2)") "^p(2)^fg()^bg()"
        , ppSep     = " ^fg(grey60)^r(1x8)^fg() "
        , ppLayout  = dzenColor colorWhite "" . (\x -> case x of
                                                            "myCode"                                    -> pad "^i(/home/ryan/.dzen/icons/layout-threecol.xbm)"
                                                            "myDish"                                    -> pad "^i(/home/ryan/.dzen/icons/layout-im-gimp.xbm)"
                                                            "myWide"                                    -> pad "^i(/home/ryan/.dzen/icons/layout-mirror-black.xbm)"
                                                            "mySplit"                                   -> pad "^i(/home/ryan/.dzen/icons/layout-tall-black.xbm)"
                                                            "myGrid"                                    -> pad "^i(/home/ryan/.dzen/icons/layout-gimp.xbm)"
                                                            "myChat"                                    -> pad "^i(/home/ryan/.dzen/icons/layout-im.xbm)"
                                                            "myLaTeX"                                   -> pad "^i(/home/ryan/.dzen/icons/layoput-gimp.xbm)"
                                                            "Full"                                      -> pad "^i(/home/ryan/.dzen/icons/layout-full.xbm)"
                                                            _                                           -> pad $ shorten 10 x
                                                   )
        , ppUrgent  = dzenColor colorDarkGray colorYellow . wrap "[" "]"
        , ppTitle   = dzenColor colorWhite "" . trim
        , ppExtras  = [dzenColorL colorYellow "". wrapL mailIcon "" . padL $ gmailCheck,
                       dzenColorL colorBrightGreen "" . wrapL mailIcon "" . padL $ queensuCheck,
                       dzenColorL colorWhite "". padL $ layoutCheck]
        , ppOutput  = hPutStrLn h
        }
    where
        -- logMail = dzenColorL colorPink "" . wrapL mailIcon "". padL $ maildirNew mailDir
        mailCheck = mailIcon -- <$> gmailCheck <$> queensuCheck
        mailIcon = "^i(/home/ryan/.dzen/icons/mail.xbm)"
        --gmailCheck =  padL $ (dzenColorL colorRed "" . logCmd "cat /home/ryan/.xmonad/gmailcheck")
        --queensuCheck = padL $ (dzenColorL colorYellow "" . logCmd "cat /home/ryan/.xmonad/queensucheck")
        gmailCheck = logCmd "cat /home/ryan/.xmonad/gmailcheck"
        queensuCheck = logCmd "cat /home/ryan/.xmonad/queensucheck"
        layoutCheck = logCmd "cat /home/ryan/.xmonad/layout"

--}}}

-- Commented out because it exposes a bug in xmonad-contrib:
--myLogHook = ewmhDesktopsLogHookCustom $ filter $ \w -> W.tag w /= "SP"


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


-- myGSW :: GSConfig Window -> X (Maybe Window)
-- myGSW gsconf = windowMap >>= gridselect gsconf

-- myGSConfig = defaultGSConfig { gs_navigate = navNSearch }
myGSConfig :: HasColorizer a => GSConfig a
myGSConfig = defaultGSConfig { gs_navigate = navNSearch }

main = do
    dzenpipe <- spawnPipe statusBarCmd
    xmonad $ withUrgencyHookC dzenUrgencyHook { args = ["-bg", "red", "-fg", "yellow", "-x", "1"] } urgencyConfig { remindWhen = Every 30 } --NoUrgencyHook
           $ defaultConfig { -- kde4Config {
      workspaces = myWorkspaces
      , modMask = myMod -- use the Windows button as mod
      , terminal = myTerminal
      , normalBorderColor  = inactiveBorderColor myTheme
      , focusedBorderColor = activeBorderColor myTheme
      , borderWidth = 1
      , layoutHook = myLayout
      , manageHook = manageHook defaultConfig <+> myManageHook -- kde4Config <+> myManageHook
      , mouseBindings      = myMouseBindings
      , keys = \x -> (M.fromList $ mykeys x) `M.union` dvorakify (keys defaultConfig x)
      --, logHook               = (dynamicLogWithPP $ mPP dzenpipe) >> updatePointer (Relative 0.95 0.95)
      , logHook               = dynamicLogWithPP $ mPP dzenpipe
      -- , logHook = myLogHook
      }
      where
          myManageHook = composeAll . concat $
                [
              --[ [ className   =? c --> doFloat           | c <- myFloats]
                [ title       =? t --> doFloat           | t <- myOtherFloats]
              , [ title       =? c --> viewShift "term" | c <- term]
              , [ className   =? c --> viewShift "term" | c <- term2]
              , [ className   =? c --> viewShift "web" | c <- web]
              , [ className   =? c --> viewShift "chatter" | c <- chatter]
            --  , [ className   =? c --> doF (W.shift "plasma") | c <- plasmaD ]
              , [ className   =? c --> doIgnore         | c <- panel ]
              , [ isFullscreen     --> doFullFloat ]
              ]
          viewShift = doF . liftM2 (.) W.greedyView W.shift
          mykeys x = [
            --      ((myMod, xK_x), spawn  myTerminal)
            --      , ((myMod, xK_c), kill)
            ((myMod, xK_Left), prevWS)
            , ((myMod, xK_Right), nextWS)
            , ((myMod .|. shiftMask, xK_Left),  shiftToPrev >> prevWS)
            , ((myMod .|. shiftMask, xK_Right), shiftToNext >> nextWS)
            , ((myMod, xK_a), myToggle)
            , ((myMod, xK_z), shellPrompt myXPConfig)
            , ((myMod, xK_g), goToSelected myGSConfig)
            , ((myMod .|. shiftMask, xK_g), gridselectWorkspace myGSConfig W.greedyView)
            , ((myMod, xK_F4), spawn "sleep 0.5 && xset dpms force suspend")
            , ((myMod, xK_F5), spawn "sleep 0.5 && xset dpms force off")
            , ((myMod, xK_F6), spawn "sleep 0.5 && /home/ryan/bin/icd")
            , ((myMod, xK_Up),   spawn "sleep 0.5 && b u")
            , ((myMod, xK_Down), spawn "sleep 0.5 && b d")
            , ((myMod, xK_l), spawn "xautolock -enable && sleep 1 && xautolock -locknow")
            , ((myMod .|. shiftMask, xK_l), spawn "xautolock -toggle")
            , ((myMod .|. myCtrl .|. shiftMask, xK_Right), sendMessage $ Move R)
            , ((myMod .|. myCtrl .|. shiftMask, xK_Left),  sendMessage $ Move L)
            , ((myMod .|. myCtrl .|. shiftMask, xK_Up),    sendMessage $ Move U)
            , ((myMod .|. myCtrl .|. shiftMask, xK_Down),  sendMessage $ Move D)
            , ((myMod, xK_BackSpace), focusUrgent)
              --      , ((myMod, xK_Tab), bindOn [("chat", rotSlavesDown), ("", rotAllDown)])
              --      , ((myMod .|. shiftMask, xK_Tab), bindOn [("chat", rotSlavesUp), ("", rotAllUp)])
            , ((myMod, xK_s), sendMessage $ ToggleLayout)
            , (
               (myMod, xK_b), submap . M.fromList $
                              [ ((m, k), f)
                              | m <- [0, myMod]
                              , (k, f) <- [ (xK_a, addWorkspacePrompt myXPConfig)
                                          , (xK_c, withWorkspace myXPConfig (windows . copy))
                                          , (xK_d, kill1)
                                          , (xK_k, removeWorkspace)
                                          , (xK_m, withWorkspace myXPConfig (windows . W.shift))
                                          , (xK_o, killAllOtherCopies)
                                          , (xK_r, renameWorkspace myXPConfig)
                                          , (xK_s, selectWorkspace myXPConfig)
                                          ]
                              ]
                )
              ]
          dvorakify kl = M.fromList $ map (\((m, k), d) -> ((m, dk k), d)) $ M.toList kl
          myFloats      = ["MPlayer", "Plasma-netbook", "Plasma-desktop", "plasma-netbook", "plasma-desktop"]
          plasmaD       = ["Plasma"]
          myOtherFloats = ["alsamixer"]
          term          = ["ryan@localhost", "root@localhost", "ryan@lambda", "ryan@kappa", "ryan@sho", "Konsole"]    -- open on desktop 1
          term2         = ["Konsole", "urxvt"]
          web           = ["Firefox", "Iceweasel", "Opera", "Akregator", "Konqueror", "Chromium-browser", "Firefox-bin", "Navigator"] -- open on desktop 2
          chatter       = ["Ksirc", "Krusader", "xchat", "Quassel", "Pidgin", "Kopete", "kopete"]      -- open on desktop 3
          games         = ["Bzflag"]
          panel         = ["panel", "trayer"]

-- Avoid the master window, but otherwise manage new windows normally.
avoidMaster :: W.StackSet i l a s sd -> W.StackSet i l a s sd
avoidMaster = W.modify' $ \c -> case c of
    W.Stack t [] (r:rs) -> W.Stack t [r] rs
    otherwise           -> c

-- A version of toggleWS that ignores the scratchPad workspaces.
myToggle = windows $ W.view =<< W.tag . head . filter ((\x -> x /= "NSP" && x /= "SP") . W.tag) . W.hidden

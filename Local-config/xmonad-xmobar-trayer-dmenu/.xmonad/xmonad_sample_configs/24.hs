-- http://haskell.org/haskellwiki/index.php?title=Xmonad/Config_archive/ivy-foster-xmonad.hs
------------------------------------------------------------
-- ~/.xmonad/xmonad.hs - xmonad configuration
------------------------------------------------------------

------------------------------------------------------------
--
-- 0. Table of Contents
-- ~~~~~~~~~~~~~~~~~~~~
--
-- 1. Imports
-- 2. Main
-- 3. Layout
-- 4. ManageHooks
-- 5. ManageHook Helpers
-- 6. Keybindings
-- 7. Extensions & Misc.
-- 7.1 runCommand
-- 7.2 Named scratchpads
-- 7.3 Misc
--
------------------------------------------------------------

------------------------------------------------------------
-- 1. Imports
------------------------------------------------------------

import Data.List
import qualified Data.Map as M
import Graphics.X11.Xlib
import System.Exit
import System.IO

import XMonad
import XMonad.Actions.Commands
import XMonad.Actions.CopyWindow
import XMonad.Actions.FindEmptyWorkspace
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.FadeInactive(setOpacity)
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Layout.LayoutHints
import XMonad.Layout.Named
import XMonad.Layout.NoBorders
import XMonad.Layout.PerWorkspace
import qualified XMonad.StackSet as W
import XMonad.Util.NamedScratchpad
import XMonad.Util.Run(spawnPipe)

------------------------------------------------------------
-- 2. Main & Logs
------------------------------------------------------------

main :: IO ()
main = do
    xmproc <- spawnPipe "xmobar /home/ivy/.xmonad/xmobar.hs"
    xmonad $ ewmh $ myConfig xmproc

myConfig xmproc = defaultConfig
                  { borderWidth        = 1
                  , focusedBorderColor = focusColor
                  , focusFollowsMouse  = False 
                  , keys               = myKeys
                  , layoutHook         = myLayout
                  , logHook            = myLogHook xmproc
                  , manageHook         = myManageHook
                  -- I put a Hyper key where Right Alt
                  -- traditionally is. Thanks, xmodmap!
                  , modMask            = mod3Mask
                  , normalBorderColor  = "#333333"
                  , terminal           = "urxvt"
                  , workspaces         = myWorkspaces
                  }

myLogHook :: Handle -> X ()
myLogHook h = do
    -- I use copyWindow pretty extensively in my ManageHooks
    copies <- wsContainingCopies
    -- If another workspace has a copy of this window, put a
    -- star next to it in the workspace bar
    let check ws | ws `elem` copies = xmobarColor unfocusColor ""
                                    . ("*" ++) $ ws
                 | otherwise = formatWorkspace unfocusColor "" $ ws
        formatWorkspace fg bg = xmobarColor fg bg
    dynamicLogWithPP $ xmobarPP
        { ppCurrent = formatWorkspace "#000000" focusColor
        , ppHidden = check
        -- Takes up less space this way
        , ppLayout = (\x -> case x of
                                "Tall" -> "|"
                                "Wide" -> "-"
                                "Full" -> "^")
        , ppOutput = hPutStrLn h
        -- I don't want NSP showing up at the end of my
        -- workspace list
        , ppSort = fmap (.namedScratchpadFilterOutWorkspace)
                   $ ppSort defaultPP
        , ppTitle = (" " ++) . xmobarColor "#35acdb" "" 
        , ppUrgent = formatWorkspace "" "red" . xmobarStrip
        , ppVisible = xmobarColor "#3579a8" ""
        }

------------------------------------------------------------
-- 3. Layout
------------------------------------------------------------

myLayout = onWorkspace "6:video" video
         $ onWorkspace "2:web" (full ||| tall ||| wide)
         $ (tall ||| wide ||| full)
    where
        nmaster = 1
        delta = 0.03
        ratio = 0.49
        full = named "Full"
             $ avoidStruts
             $ smartBorders
             $ layoutHintsWithPlacement (0.5, 0.5)
             $ Full
        tall = named "Tall"
             $ avoidStruts
             $ smartBorders
             $ layoutHintsWithPlacement (0.5, 0.5)
             $ Tall nmaster delta ratio
        video = named "Full"
              $ smartBorders
              $ layoutHintsWithPlacement (0.5, 0.5)
              $ Full
        wide = named "Wide"
             $ avoidStruts
             $ smartBorders
             $ layoutHintsWithPlacement (0.5, 0.5)
             $ Mirror (Tall nmaster delta ratio)

myWorkspaces :: [WorkspaceId]
myWorkspaces = [ "1:main"
               , "2:web"
               , "3:emacs"
               , "4:docs"
               , "5:images"
               , "6:video"
               , "7:term"
               , "8:gimp"
               , "9:scratch"
               ]

------------------------------------------------------------
-- 4. ManageHooks
------------------------------------------------------------

myManageHook :: ManageHook
myManageHook = composeAll
    [ manageConkeror
    , manageDocs
    , manageEmacs
    , manageGimp
    , manageImages
    , manageTerm
    , manageTransient
    , manageVideo
    , manageWeb
    , myNSManageHook scratchpads
    ]

manageConkeror :: ManageHook
manageConkeror = 
    className =? "Conkeror"
    --> composeOne
        [ ("*" `isPrefixOf`) `fmap` title -?> doF W.focusDown
        , stringProperty "WM_WINDOW_ROLE" =? "console" -?> doFlopDown ]

manageDocs :: ManageHook
manageDocs = composeOne
    [ className =? c -?> doShift "4:docs" 
    | c <- [ "Xpdf"
           , "Zathura"
           ]]

-- liftX lifts a normal X action into a Query (as expected by -->)
-- idHook ensures the proper return type
manageEmacs :: ManageHook
manageEmacs =
    className =? "Emacs"
    --> (ask >>= doF . \w -> (copyWindow w "3:emacs"))
    <+> (ask >>= \w -> liftX (setOpacity w 0.9) >> idHook)

manageGimp :: ManageHook
manageGimp =
    className =? "Gimp"
    --> (doShift "8:gimp")
    <+> composeOne
         [ stringProperty "WM_WINDOW_ROLE" =? "gimp-image-window"
            -?> doCenterFloat
         , stringProperty "WM_WINDOW_ROLE" =? "gimp-image-new"
            -?> doCenterFloat
         , stringProperty "WM_WINDOW_ROLE" =? "gimp-dock"
            -?> doTopRightFloat
         , stringProperty "WM_WINDOW_ROLE" =? "gimp-toolbox"
            -?> doTopLeftFloat
         , stringProperty "WM_WINDOW_ROLE" =? "gimp-toolbox-color-dialog"
            -?> doBottomRightFloat
         , return True -?> doFloat ]

manageImages :: ManageHook
manageImages = composeOne
    [ className =? c
    -?> doTopLeftFloat
    <+> (ask >>= doF . \w -> (copyWindow w "5:images"))
    | c <- [ "feh"
           , "Display"
           ]]

manageTerm :: ManageHook
manageTerm = composeOne
    [ className =? c
    -?> (ask >>= \w -> liftX (setOpacity w 0.9) >> idHook)
    <+> (ask >>= doF . \w -> (copyWindow w "7:term"))
    | c <- [ "URxvt"
           , "XTerm"
           ]]

manageTransient :: ManageHook
manageTransient = 
    composeAll 
        [ (className =? "Xmessage" 
                     --> doCenterFloat
                     <+> (ask >>= \w -> liftX (setOpacity w 0.7) >> idHook))
        , (stringProperty "WM_NAME" =? "xine Panel"
                     --> doBottomRightFloat
                     <+> (ask >>= \w -> liftX (setOpacity w 0.7) >> idHook))
        ]

manageVideo :: ManageHook
manageVideo = composeOne
    [ className =? c -?> (ask >>= doF . \w -> (copyWindow w "6:video"))
    | c <- [ "MPlayer"
           , "xine"
           ]]

manageWeb :: ManageHook
manageWeb = composeOne
    [ className =? c -?> (ask >>= doF . \w -> (copyWindow w "2:web"))
    | c <- [ "Conkeror"
           , "Dillo"
           , "Firefox"
           ]]

------------------------------------------------------------
-- 5. ManageHook Helpers
------------------------------------------------------------

doFlopDown :: ManageHook
doFlopDown = doF (W.focusUp . W.swapDown)

doTopRightFloat :: ManageHook
doTopRightFloat = ask
                >>= \w -> doF . W.float w . position . snd
                =<< liftX (floatLocation w)
    where
    position (W.RationalRect _ _ w h) = W.RationalRect (1-w) 0.03 w h

doTopLeftFloat :: ManageHook
doTopLeftFloat = ask
                >>= \w -> doF . W.float w . position . snd
                =<< liftX (floatLocation w)
    where
    position (W.RationalRect _ _ w h) = W.RationalRect 0 0.03 w h

doBottomRightFloat :: ManageHook
doBottomRightFloat = ask
                    >>= \w -> doF . W.float w . position . snd
                    =<< liftX (floatLocation w)
    where
    position (W.RationalRect _ _ w h) = W.RationalRect (1-w) (1-h) w h

doBottomLeftFloat :: ManageHook
doBottomLeftFloat = ask
                    >>= \w -> doF . W.float w . position . snd
                    =<< liftX (floatLocation w)
    where
    position (W.RationalRect _ _ w h) = W.RationalRect 0 (1-h) w h

------------------------------------------------------------
-- 6. Keybindings
------------------------------------------------------------

-- This is probably overkill.
myKeys :: XConfig Layout -> M.Map (KeyMask, KeySym) (X ())
myKeys conf@(XConfig {XMonad.modMask = modMask}) = M.fromList $
    -- Launching & killing
    [ ((modMask,               xK_Return), runOrCopy (XMonad.terminal conf) (className =? "URxvt"))
    , ((modMask .|. controlMask, xK_Return), spawn $ XMonad.terminal conf)
    , ((modMask,               xK_p     ), spawn "/home/ivy/bin/dmenu_run -b")
    , ((modMask .|. shiftMask, xK_c     ), kill1) -- $copies && rm copy || kill process
    , ((modMask .|. shiftMask, xK_e     ), runOrCopy "emacsclient -c" (className =? "Emacs"))
    , ((modMask,               xK_y     ), commands >>= runCommand)
    , ((0,                     xK_Pause ), spawn "mpc toggle")
    , ((shiftMask,             xK_Pause ), spawn "mpc next")
    , ((controlMask,           xK_Pause ), spawn "mpc prev")
    , ((modMask,               xK_b     ), runOrCopy "conkeror" (className =? "Conkeror"))
    -- Scratchpads
    , ((modMask,               xK_s     ), namedScratchpadAction scratchpads "term")
    , ((modMask .|. shiftMask, xK_s     ), namedScratchpadAction scratchpads "music")
    , ((modMask .|. controlMask, xK_s   ), namedScratchpadAction scratchpads "volume")
    -- Windows
    , ((modMask,               xK_space ), sendMessage NextLayout)
    , ((modMask .|. shiftMask, xK_space ), setLayout $ XMonad.layoutHook conf)
    -- Workspaces
    , ((modMask,               xK_m     ), viewEmptyWorkspace)
    , ((modMask .|. shiftMask, xK_m     ), tagToEmptyWorkspace)
    , ((modMask,               xK_v     ), windows copyToAll)
    , ((modMask .|. shiftMask, xK_v     ), killAllOtherCopies)
    -- Window focus
    , ((modMask,               xK_j     ), windows W.focusDown)
    , ((modMask,               xK_k     ), windows W.focusUp)
    -- Swap windows
    , ((modMask .|. shiftMask, xK_Return), windows W.swapMaster)
    , ((modMask .|. shiftMask, xK_j     ), windows W.swapDown  )
    , ((modMask .|. shiftMask, xK_k     ), windows W.swapUp    )
    -- Size ratios 
    , ((modMask,               xK_h     ), sendMessage Shrink)
    , ((modMask,               xK_l     ), sendMessage Expand)
    -- Push floats back into tiling
    , ((modMask,               xK_t     ), withFocused $ windows . W.sink)
    -- Number of windows in the master pane
    , ((modMask,               xK_comma ), sendMessage (IncMasterN 1))
    , ((modMask,               xK_period), sendMessage (IncMasterN (-1)))
    -- quit/restart
    , ((modMask .|. shiftMask, xK_q     ), io (exitWith ExitSuccess)) -- %! Quit xmonad
    , ((modMask,               xK_q     ), restart "xmonad" True) -- %! Restart xmonad
    ]
    ++
    -- mod-[1..9] %! Switch to workspace N
    -- mod-shift-[1..9] %! Move client to workspace N
    -- mod-mod1-[1..9] %! Copy client to workspace N
    [((m .|. modMask, k), windows $ f i)
        | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9]
        , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask), (copy, mod1Mask)]]

------------------------------------------------------------
-- 7. Extensions & Misc.
------------------------------------------------------------

-- 7.1 runCommand
-- ~~~~~~~~~~~~~~

commands :: X [(String, X ())]
commands = defaultCommands

-- 7.2 Named scratchpads
-- ~~~~~~~~~~~~~~~~~~~~~

scratchpads :: [NamedScratchpad]
scratchpads = 
    [ NS "music" "urxvt -title Music -e ncmpc"
             (title =? "Music")
             (customFloating $ W.RationalRect (1/6) (1/6) (2/3) (2/3))
    , NS "term" "urxvt -title Scratchpad -e tmux -uLmain attach -t1"
             (title =? "Scratchpad")
             (customFloating $ W.RationalRect (1/6) (1/6) (2/3) (2/3))
    , NS "volume" "pavucontrol"
             (className =? "Pavucontrol")
             (customFloating $ W.RationalRect (1/4) (1/4) (2/4) (2/4))
    ]

-- I like to have these floating windows transparent
myNSManageHook :: NamedScratchpads -> ManageHook
myNSManageHook s =
    namedScratchpadManageHook s
    <+> composeOne
            [ title =? "Music"
              -?> (ask >>= \w -> liftX (setOpacity w 0.7) >> idHook)
            , title =? "Scratchpad"
              -?> (ask >>= \w -> liftX (setOpacity w 0.7) >> idHook)
            , className =? "Pavucontrol"
              -?> (ask >>= \w -> liftX (setOpacity w 0.8) >> idHook)
            ]

-- 7.3 Misc.
-- ~~~~~~~~~

focusColor :: String
focusColor = "#3579a8"

unfocusColor :: String
unfocusColor = "#ffffff"

-- End of file

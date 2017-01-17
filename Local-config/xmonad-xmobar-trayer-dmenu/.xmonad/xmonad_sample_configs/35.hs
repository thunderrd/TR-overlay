-- http://haskell.org/haskellwiki/index.php?title=Xmonad/Config_archive/Regalia%27s_xmonad.hs
import XMonad
import qualified XMonad.StackSet as W
import qualified Data.Map        as M
import Control.Monad
import System.IO
import System.IO.Unsafe
import Data.List
import Data.Ratio ((%))

-- Actions
import XMonad.Actions.CycleWS
import XMonad.Actions.Plane
import XMonad.Actions.TopicSpace
import XMonad.Actions.SpawnOn
import XMonad.Actions.OnScreen
import XMonad.Actions.SwapWorkspaces

-- Hooks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.SetWMName
import XMonad.Hooks.FadeInactive
import XMonad.Hooks.UrgencyHook hiding (Never)

-- Layouts
import XMonad.Layout.PerWorkspace
import XMonad.Layout.IM
import XMonad.Layout.Grid
import XMonad.Layout.TwoPane
import XMonad.Layout.NoBorders
import XMonad.Layout.Reflect

-- Prompts
import XMonad.Prompt
import XMonad.Prompt.Input
import XMonad.Prompt.Workspace
import XMonad.Prompt.Shell
import XMonad.Prompt.XMonad
import XMonad.Prompt.RunOrRaise
import XMonad.Prompt.AppendFile

-- Util
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.EZConfig(additionalKeysP, additionalKeys)

--
-- main
--

main = do
    checkTopicConfig myTopics myTopicConfig
    dzen <- spawnPipe myStatusBar
    other <- spawnPipe myLeft
    other <- spawnPipe myRight
    xmonad $ withUrgencyHook NoUrgencyHook defaultConfig
	{ manageHook         = manageHook defaultConfig <+> myManageHook 
	, layoutHook         = mylayoutHook
	, startupHook        = setWMName "LG3D"
	, terminal           = myTerminal
	, modMask            = myModMask
	, borderWidth        = myBorderWidth
	, focusFollowsMouse  = True
	, normalBorderColor  = myBorder
	, focusedBorderColor = myFocusedBorder
	, workspaces         = myTopics 
	, logHook            = myLogHook >> (dynamicLogWithPP $ myDzenPP dzen)
	} `additionalKeysP` myKeys

-- End Main

-- Simple configuration
myBorderWidth = 2
myBrowser = "firefox"
myTerminal = "urxvtc"
myShell = "zsh"
myModMask = mod4Mask
myIconDir = "/home/scott/.dzen/dzenIcons/"
myStatusBar = "dzen2 -x '0' -y '0' -h '20' -w '330' -ta 'l' -bg '" ++ myDBGColor ++ "' -fn '" ++ myFont ++ "'"
myLeft = ".dzen/left.zsh | dzen2 -xs 1 -x '330' -y '0' -h '20' -w '1110' -ta 'r' -bg '" ++ myDBGColor ++ "' -fg '" ++ myDFGColor ++ "' -fn '" ++ myFont ++ "'"
myRight = ".dzen/right.zsh | dzen2 -xs 2 -y '0' -h '20' -ta 'r' -bg '" ++ myDBGColor ++ "' -fg '" ++ myDFGColor ++ "' -fn '" ++ myFont ++ "'"
myFont = "-*-terminus-medium-*-*-*-12-120-75-75-*-*-iso8859-*"

-- Layout Hook
mylayoutHook = smartBorders $ avoidStruts $ lessBorders (Combine Difference Screen OnlyFloat) (Mirror tiled ||| tiled ||| fullscreenLayout)
    where
	fullscreenLayout = smartBorders Full
	tiled = Tall nmaster delta ratio
	nmaster = 1
	delta = 3 / 100
	ratio = 11 / 20

-- Manage hook
myManageHook = composeAll
    [  className =? "Xmessage"  --> doFloat 
    ,  className =? "Pidgin"    --> doShift "im"      
    ,  className =? "Navigator" --> doShift "web"      
    ,  title	 =? "wowee"     --> doShift "term"
    ,  title	 =? "dev"       --> doShift "dev"
    ,  title     =? "irssi"     --> doShift "irc"
    ,  title     =? "nyxmms2"   --> doShift "music"
    ,  title     =? "sup"       --> doShift "mail"
    ,  title     =? "htop"      --> doShift "mon"
    ] 
		<+> (fmap not isDialog --> doF avoidMaster)
		<+> composeOne [ isFullscreen -?> doFullFloat ]

-- Log Hook
myLogHook = fadeInactiveLogHook fadeAmount
    where fadeAmount = 0.90

-- XP Config
myXPConfig :: XPConfig
myXPConfig = defaultXPConfig { font = myFont
		             , height = 22
			     , bgColor = myDBGColor }

myDFGColor = "#b2b27f" -- Dzen
myDBGColor = "#2f3436"
myFFGColor = "#4c5e52" -- Focused
myFBGColor = "#ffbe2c"
myVFGColor = "#4c5e52" -- Visible
myVBGColor = "#2f3436"
myUFGColor = "#4c5e52" -- Urgent
myUBGColor = "#ffeca1"
myIFGColor = "#ffeca1" -- Icon
myIBGColor = myDBGColor
mySColor   = myDFGColor -- Seperator
myBorder   = "#4c5e52"
myFocusedBorder = "#4c5e52"

-- Pretty Printing
myDzenPP h = defaultPP
     {  ppCurrent         = dzenColor myFFGColor myFBGColor . wrap ("^fg(" ++ myIFGColor ++ ")^i(" ++ myIconDir ++ "/eye_l.xbm)" ++ "^fg(" ++ myFFGColor ++ ")") "" 
      , ppVisible         = dzenColor myVFGColor myVBGColor . wrap "" ("^fg(" ++ myIFGColor ++ ")^i(" ++ myIconDir ++ "/eye_r.xbm)")
      , ppHidden          = dzenColor myDFGColor myDBGColor . wrap ("^i(" ++ myIconDir ++ "/dzen_bitmaps/has_win.xbm)") ""
      , ppHiddenNoWindows = dzenColor myDFGColor myDBGColor . wrap ("^i(" ++ myIconDir ++ "/dzen_bitmaps/has_win_nv.xbm)") ""
      , ppUrgent          = dzenColor myUFGColor myUBGColor . wrap ("^i(" ++ myIconDir ++ "/info_03.xbm)") "" . dzenStrip
      , ppTitle           = dzenColor myDFGColor myDBGColor . shorten 0
      , ppLayout          = dzenColor myDFGColor myDBGColor .
                            (\x -> case x of
                            "Mirror Tall" -> "^fg(" ++ myIFGColor ++ ")^i(" ++ myIconDir ++ "/dzen_bitmaps/mtall.xbm)"
                            "Tall"	  -> "^fg(" ++ myIFGColor ++ ")^i(" ++ myIconDir ++ "/dzen_bitmaps/tall.xbm)"
                            "Full"	  -> "^fg(" ++ myIFGColor ++ ")^i(" ++ myIconDir ++ "/dzen_bitmaps/full.xbm)"
                            "Grid"	  -> "^fg(" ++ myIFGColor ++ ")^i(" ++ myIconDir ++ "/dzen_bitmaps/grid.xbm)"
                            "TwoPane"	  -> "^fg(" ++ myIFGColor ++ ")^i(" ++ myIconDir ++ "/dzen_bitmaps/two_pane.xbm)"
                            _ -> x
                            )
      , ppSep             = " "
      , ppOutput          = hPutStrLn h }

-- Key Bindings 
myKeys = [ ("M-f",	spawn myBrowser)  	
         -- xmms2
         , ("M-S-'",	spawn "nyxmms2 prev")
         , ("M-S-.",	spawn "nyxmms2 next")
         , ("M-S-p",	spawn "nyxmms2 toggle")
	 -- ALSA Volume
         , ("M-C-e",	spawn "amixer set Master 2%-")
         , ("M-C-u",	spawn "amixer set Master 2%+")
         -- CycleWS
         , ("M-s",	nextWS)
         , ("M-n",	prevWS)
	 -- Swap Workspaces
	 , ("M-S-s",	swapTo Next)
	 , ("M-S-n",	swapTo Prev)
         -- Prompt
         , ("M-C-p",	runOrRaisePrompt defaultXPConfig)
         , ("M-C-.",	shellPrompt defaultXPConfig)
         , ("M-C-j",	jumpPrompt)
	 -- Toggle Struts
	 , ("M-S-s",	sendMessage ToggleStruts)
	 -- Topics
	 , ("M-a",	currentTopicAction myTopicConfig)
	 , ("M-g",	promptedGoto)
	 , ("M-c",	promptedGotoOtherScreen)
	 , ("M-S-g",	promptedShift)
         -- Restart
         , ("M-q",	spawn myRestart)
	 ]
         ++	
         -- Change Xinerama bindings
         [ ("M-"++key, screenWorkspace sc >>= flip whenJust (windows . f))
             | (key, sc) <- zip ["w", "v", "z"] [0..]
             , (f, m) <- [(W.view, 0)]] 

spawnShell :: X ()
spawnShell = currentTopicDir myTopicConfig >>= spawnShellIn

spawnShellIn :: Dir -> X ()
spawnShellIn dir = spawn $ myTerminal ++ " -title urxvt -e sh -c 'cd ''" ++ dir ++ "'' && " ++ myShell ++ "'"

goto :: Topic -> X ()
goto = switchTopic myTopicConfig
	
promptedGoto :: X ()
promptedGoto = workspacePrompt myXPConfig goto

promptedGotoOtherScreen :: X ()
promptedGotoOtherScreen =
	workspacePrompt myXPConfig $ \ws -> do
		nextScreen
		goto ws

promptedShift :: X ()
promptedShift = workspacePrompt myXPConfig $ windows . W.shift

jumpPrompt :: X ()
jumpPrompt = inputPrompt defaultXPConfig ("Jump") ?+ spawnJump

spawnJump ::  String -> X ()
spawnJump s = spawn ("nyxmms2 jump artist:" ++ s) 

-- Helper functions
--
-- Avoid changing master on new window creation
avoidMaster :: W.StackSet i l a s sd -> W.StackSet i l a s sd
avoidMaster = W.modify' $ \c -> case c of
	W.Stack t [] (r:rs) -> W.Stack t [r] rs
	otherwise			-> c

-- Kill zombie dzens before normal xmonad restart
myRestart :: String
myRestart = "for pid in `pgrep dzen2`; do kill -9 $pid; done && xmonad --recompile && xmonad --restart"

-- TopicSpace things
myTopics :: [Topic]
myTopics =
  [   "web"
    , "mail"
    , "term"
    , "music"
    , "im"
    , "irc"
    , "dev"
    , "mon"
    ]

myTopicConfig :: TopicConfig
myTopicConfig = TopicConfig
	{ topicDirs = M.fromList $
	[ ("music", "~/music")
	]
	, defaultTopicAction = const $ spawnShell
	, defaultTopic = "web"
	, maxTopicHistory = 10
	, topicActions = M.fromList $
		[ ("web",	spawn myBrowser)
		, ("mail",	spawn "urxvtc -e sup")
		, ("term",	spawnShell )
		, ("im",	spawn "pidgin")
		, ("music",	spawn "urxvtc -e nyxmms2")
		, ("irc",	spawn "urxvtc -e irssi")
		, ("dev",	spawnShell )
		, ("mon",	spawn "urxvtc -e htop")
		]
	}

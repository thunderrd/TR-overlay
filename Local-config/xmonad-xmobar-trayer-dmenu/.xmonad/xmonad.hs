-- ~/.xmonad/xmonad.hs


-- these are the utility modules to import
import XMonad

import Data.Monoid
import System.Exit
import qualified XMonad.StackSet as W
import qualified Data.Map        as M
import XMonad.Actions.SpawnOn           	--for startupHook
import XMonad.Hooks.DynamicLog
import XMonad.Util.Font
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers (isFullscreen, doFullFloat, doCenterFloat, doFloatAt)
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.EZConfig(additionalKeys)
import System.IO
import Text.Regex.Posix



-- defines workspace names
myWorkspaces = ["shell", "fah", "web", "irc", "files"] ++ map show [6..9]

-- where do we want to be after XMonad starts, and apps are spawned
startupWorkspace = "web"




--  which apps should have floating windows? ----------------------------------
myManageHook = composeAll
	[ 
	  className =? "gimp" --> doFloat 					--doFloat floats child windows of the parent application
	, className =? "hexchat" --> doShift "irc"				--doShift shifts all windows of app 'x' to WS 'y'
	, className =? "qjackctl" --> doShift "9"
	, className =? "vlc" --> doShift "9"
	, className =? "Leafpad" --> doFloatAt 0.025 0.09
	, className =? "Gcr-prompter" --> doFloatAt 0.025 0.09
	, appName =? "gkrellm" --> doFloatAt 0.792 0.018


-- the following are WM_* properties found with xprop, child windows (position specified here so as not to cover xmobar space)

 	, stringProperty "WM_NAME" =? "File Operation Progress" --> doFloatAt 0.55 0.25		--thunar child

 	, stringProperty "WM_WINDOW_ROLE" =? "prefs" --> doFloatAt 0.1 0.1				--hexchat children
 	, stringProperty "WM_WINDOW_ROLE" =? "ChanList" --> doFloatAt 0.1 0.1
 	, stringProperty "WM_WINDOW_ROLE" =? "servlist" --> doFloatAt 0.1 0.1

 	, stringProperty "WM_WINDOW_ROLE" =? "Preferences" --> doFloatAt 0.1 0.1		--firefox children
 	, stringProperty "WM_WINDOW_ROLE" =? "Organizer" --> doFloatAt 0.1 0.1
 	, stringProperty "WM_NAME" =? "About Mozilla Firefox" --> doFloatAt 0.1 0.1


-- 	, stringProperty "WM_WINDOW_ROLE" =? "Progress" --> doFloatAt 0.1 0.1			--torbrowser
-- 	, stringProperty "WM_NAME" =? "Torbutton Preferences" --> doFloatAt 0.1 0.1
-- 	, stringProperty "WM_NAME" =? "HTTPS Everywhere Preferences" --> doFloatAt 0.1 0.1
-- 	, stringProperty "WM_NAME" =? "NoScript Options" --> doFloatAt 0.1 0.1
-- 	, stringProperty "WM_NAME" =? "About Tor Browser" --> doFloatAt 0.1 0.1


 	, stringProperty "WM_NAME" =? "GKrellM Configuration" --> doFloatAt 0.1 0.1		--GKrellM


	, stringProperty "WM_NAME" =? "PokerTH 1.1.1" -->doFloatAt 0.4 0.4				--pokerth

	-- add more here as needed
	]

-----------------------------------------------------------------
-----------------------------------------------------------------

main = do
    xmproc <- spawnPipe "xmobar"				     			--starts xmobar, passes CL arg path to config file

    xmonad $ defaultConfig 
	{         
	  workspaces         = myWorkspaces

        , terminal           = "urxvt"				     				--specifies terminal program

        , borderWidth        =  1									--width of window borders, value of 0 disables all borders
        , normalBorderColor  = "#000000"			    			--color of unfocused window border  #444444
        , focusedBorderColor = "#636363"						--"#B87333"    --"#C76114"	--color of focused window border

        , focusFollowsMouse = False

        , manageHook = manageHook defaultConfig
		   <+> manageDocks
		   <+> myManageHook
		   <+> manageSpawn


        , layoutHook = avoidStruts  $  layoutHook defaultConfig

        , logHook = dynamicLogWithPP xmobarPP                        			--logHook uses hPutStrLn xmproc; pipes data to xmobar
	{
			  ppOutput = hPutStrLn xmproc
                        , ppTitle = xmobarColor "#0EBFE9" "" . shorten 100  	--100 characters of window title in xmobar
			, ppOrder = \(ws:_:rest) -> ws:rest						--remove line to add in window layout info: 'tall/mirror/full'
	}                                                  


-- startuphooks to start app on specific workspace automatically, with Xmonad startup

	, startupHook = do

--		spawnOn "irc" "hexchat"
	      	spawnOn "shell" "urxvt"
	      	spawnOn "fah" "urxvt"
	      	spawnOn "fah" "gkrellm"
	      	spawnOn "files" "thunar"
		spawnOn "web" "palemoon 1>~/tmp/palemoon.log"
		spawnOn "9" "qjackctl"
	      	spawnOn "9" "sleep 4; exec vlc >~/tmp/vlc.log 2>&1"			--sleep 4 delays the spawn by 4 seconds (may not actually be necessary)
	      	windows $ W.view startupWorkspace 						--looks like this line has to be last, to be where we want to be after xmonad starts
	}

		`additionalKeys` myKeys                                         

------------------------------------------------------------------
------------------------------------------------------------------

-- Key bindings. Add, modify or remove key bindings here.

myKeys =

    [
-- launch dmenu
--   ((mod1Mask,   xK_p), spawn "dmenu_run -nb '#535148' -nf 'burlywood' -fn 'xft:Droid Sans:style=Bold:pixelsize=13:antialias=true:autohint=true' -sb '#535148' -sf 'lemonchiffon'")   -- -sb 'brown'   -sf 'black'
    ((mod1Mask,   xK_p), spawn "dmenu_run -nb '#535148' -nf 'burlywood' -fn 'xft:Go:style=Bold Italic:pixelsize=13:antialias=true:autohint=true' -sb '#535148' -sf 'lemonchiffon'")   -- -sb 'brown'   -sf 'black'

-- take a full-screen screenshot
    , ((mod1Mask,   xK_Print     ), spawn "/usr/bin/screenshot scr")

-- [add other bindings here]

     ]

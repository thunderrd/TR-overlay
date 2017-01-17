-- http://haskell.org/haskellwiki/index.php?title=Xmonad/Config_archive/eschulte_xmonad.hs
-- Configuration for running xmonad as the window manager over XFCE
-- see http://xmonad.org/xmonad-docs/xmonad-contrib/XMonad-Doc-Extending.html
import XMonad
import XMonad.Layout.NoBorders        -- to remove window borders
import XMonad.Hooks.ManageDocks       -- manage docks and panels
import qualified XMonad.StackSet as W -- to shift and float windows
import qualified Data.Map as M        -- used to add key bindings

myManageHook =  composeAll
                -- per-window options, use `xprop' to learn window names and classes
                [ className =? "MPlayer"        --> doFloat
                , className =? "Gimp"           --> doFloat
                , title     =? "EPresent"       --> doFloat
                ]

myKeys conf@(XConfig {XMonad.modMask = modm}) =
    [ ((modm, xK_b     ), sendMessage ToggleStruts) ] -- Mod-b: toggle XFCE panel
newKeys x  = M.union (keys defaultConfig x) (M.fromList (myKeys x))

main = xmonad defaultConfig
       { modMask = mod4Mask   -- use the super key for xmonad commands
       , manageHook = manageDocks <+> myManageHook
       , keys = newKeys
       , layoutHook = noBorders $ avoidStruts $ layoutHook defaultConfig
       }

-- http://haskell.org/haskellwiki/index.php?title=Xmonad/Config_archive/David_Roundy%27s_xmonad.hs
module Main (main) where

import XMonad ( xmonad )
import XMonad.Config.Droundy ( config )

main :: IO ()
main = xmonad config


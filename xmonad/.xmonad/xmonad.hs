import           XMonad

import           XMonad.Hooks.DynamicLog
import           XMonad.Hooks.EwmhDesktops
import           XMonad.Layout.Magnifier
import           XMonad.Layout.ThreeColumns
import           XMonad.Util.EZConfig
import           XMonad.Util.Ungrab

main :: IO ()
main = xmonad . ewmh =<< xmobar myConfig

myConfig =
  def { modMask         = mod4Mask
      , layoutHook      = myLayout
      , handleEventHook = handleEventHook def <+> fullscreenEventHook
      }  -- Rebind Mod to the Super key
    `additionalKeysP` [ ("M-S-z", spawn "xsecurelock")
                      , ( "M-S-="
                        , unGrab
                          *> spawn
                               "scrot -s '/home/ghostpepper/Pictures/%Y-%m-%d_$p_scrot.png'"
                        )
                      , ("M-]"       , spawn "google-chrome-stable")
                      , ("M-S-return", spawn "alacritty")
                      ]

myLayout = tiled ||| Mirror tiled ||| Full ||| threeCol
 where
  threeCol = magnifiercz' 1.3 $ ThreeColMid nmaster delta ratio
  tiled    = Tall nmaster delta ratio
  nmaster  = 1      -- Default number of windows in the master pane
  ratio    = 1 / 2    -- Default proportion of screen occupied by master pane
  delta    = 3 / 100  -- Percent of screen to increment by when resizing panes

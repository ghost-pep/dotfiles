import           XMonad

import           XMonad.Hooks.DynamicLog
import           XMonad.Hooks.EwmhDesktops
import           XMonad.Hooks.ManageHelpers
import           XMonad.Layout.Magnifier
import           XMonad.Layout.ThreeColumns
import           XMonad.Util.EZConfig
import           XMonad.Util.Ungrab

main :: IO ()
main = xmonad . ewmh =<< xmobar myConfig

myConfig =
  def { modMask         = mod4Mask
      , borderWidth     = 0
      , layoutHook      = myLayout
      , manageHook      = myManageHook
      , handleEventHook = handleEventHook def <+> fullscreenEventHook
      }  -- Rebind Mod to the Super key
    `additionalKeysP` [ ("M-S-z", spawn "xsecurelock")
                      , ( "M-S-="
                        , unGrab *> spawn
                          "scrot -s \"$HOME/Pictures/%Y-%m-%d_\\$p_scrot.png\""
                        )
                      , ("M-]"                   , spawn "google-chrome-stable")
                      , ("M-S-<Return>"          , spawn "alacritty")
                      , ("<XF86AudioRaiseVolume>", spawn "pamixer -i 10")
                      , ("<XF86AudioLowerVolume>", spawn "pamixer -d 10")
                      , ("<XF86MonBrightnessUp>" , spawn "brightnessctl s +10%")
                      , ( "<XF86MonBrightnessDown>"
                        , spawn "brightnessctl s 10%-"
                        )
                      ]

myManageHook :: ManageHook
myManageHook = composeAll
  [ className =? "Gimp" --> doFloat
  , isDialog --> doFloat
  , className =? "wired" --> doFloat
  ]

myLayout = tiled ||| Mirror tiled ||| Full ||| threeCol
 where
  threeCol = magnifiercz' 1.3 $ ThreeColMid nmaster delta ratio
  tiled    = Tall nmaster delta ratio
  nmaster  = 1      -- Default number of windows in the master pane
  ratio    = 1 / 2    -- Default proportion of screen occupied by master pane
  delta    = 3 / 100  -- Percent of screen to increment by when resizing panes
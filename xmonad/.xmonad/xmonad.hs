import           XMonad

import           XMonad.Hooks.DynamicLog
import           XMonad.Hooks.EwmhDesktops
import           XMonad.Hooks.ManageHelpers
import           XMonad.Util.EZConfig
import           XMonad.Util.Ungrab

main :: IO ()
main = xmonad . ewmh =<< statusBar "xmobar" myXmobarPP toggleStrutsKey myConfig
 where
  toggleStrutsKey :: XConfig Layout -> (KeyMask, KeySym)
  toggleStrutsKey XConfig { modMask = m } = (m, xK_b)

myConfig =
  def { modMask         = mod4Mask
      , borderWidth     = 0
      , layoutHook      = myLayout
      , manageHook      = myManageHook
      , workspaces      = myWorkspaces
      , handleEventHook = handleEventHook def <+> fullscreenEventHook
      }  -- Rebind Mod to the Super key
    `additionalKeysP` [ ("M-S-z", spawn "xsecurelock")
                      , ( "M-S-="
                        , unGrab *> spawn
                          "scrot -s \"$HOME/Pictures/%Y-%m-%d_\\$p_scrot.png\""
                        )
                      , ("M-]"                   , sendMessage NextLayout)
                      , ("M-S-<Return>"          , spawn "alacritty")
                      , ("M-<Space>"             , spawn "rofi -show drun")
                      , ("<XF86AudioRaiseVolume>", spawn "pamixer -i 10")
                      , ("<XF86AudioLowerVolume>", spawn "pamixer -d 10")
                      , ("<XF86AudioMute>"       , spawn "pamixer -t")
                      , ("<XF86AudioPlay>"       , spawn "playerctl play")
                      , ("<XF86AudioPause>"      , spawn "playerctl pause")
                      , ("<XF86AudioNext>"       , spawn "playerctl next")
                      , ("<XF86AudioPrev>"       , spawn "playerctl previous")
                      , ("<XF86MonBrightnessUp>" , spawn "brightnessctl s +10%")
                      , ( "<XF86MonBrightnessDown>"
                        , spawn "brightnessctl s 10%-"
                        )
                      ]

myXmobarPP :: PP
myXmobarPP = def { ppSep           = magenta " • "
                 , ppTitleSanitize = xmobarStrip
                 , ppCurrent       = blue . wrap (green "[") (green "]")
                 , ppUrgent        = red . wrap (yellow "!") (yellow "!")
                 , ppOrder         = \[ws, _, win] -> [ws, win]
                 }
 where
  blue, magenta, red, yellow, green :: String -> String
  magenta = xmobarColor "#c9b4cf" ""
  blue    = xmobarColor "#81a2be" ""
  yellow  = xmobarColor "#f0c674" ""
  red     = xmobarColor "#cc6666" ""
  green   = xmobarColor "#b5bd68" ""

myManageHook :: ManageHook
myManageHook = composeAll
  [ className =? "Gimp" --> doFloat
  , isDialog --> doFloat
  , isFullscreen --> doFullFloat
  , className =? "wired" --> doFloat
  ]

myLayout = tiled ||| Mirror tiled ||| Full
 where
  tiled   = Tall nmaster delta ratio
  nmaster = 1      -- Default number of windows in the master pane
  ratio   = 1 / 2    -- Default proportion of screen occupied by master pane
  delta   = 3 / 100  -- Percent of screen to increment by when resizing panes

myWorkspaces = ["Web", "Edit", "Msg", "Music"] ++ map show [5 .. 9]

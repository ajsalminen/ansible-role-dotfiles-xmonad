module Main (main) where

import XMonad

import qualified Data.Map as M
import Graphics.X11.Xlib
import Graphics.X11.ExtraTypes.XF86

-- needed by CopyWindows bindings
import qualified XMonad.StackSet as W
import XMonad.Actions.CopyWindow

-- XMobar
import XMonad.Util.Run(spawnPipe)
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.DynamicLog
import System.IO
import XMonad.Util.EZConfig(additionalKeys)



main :: IO ()
main = do
  xmproc <- spawnPipe "/usr/bin/xmobar ~/.xmobarrc"
  xmonad $ defaultConfig{
     keys = myKeys <+> keys defaultConfig
    , terminal = "terminator"
    , workspaces = ["1w", "2w", "3w" , "4d", "5d", "6m", "7p", "8", "9" ]
    , modMask = mod4Mask
    , manageHook = myManageHook <+> manageHook defaultConfig
    , layoutHook = avoidStruts  $  layoutHook defaultConfig
    , logHook = dynamicLogWithPP xmobarPP
                         { ppOutput = hPutStrLn xmproc
                         , ppTitle = xmobarColor "green" "" . shorten 50
                         }
    } `additionalKeys`
    [ ((mod4Mask .|. shiftMask, xK_z), spawn "xscreensaver-command -lock")
    , ((mod4Mask, xK_b), sendMessage ToggleStruts)
    , ((mod4Mask, xK_n), spawn "~/bin/pomodoro-stop")
    , ((mod4Mask, xK_v), spawn "~/bin/pomodoro-interrupt")
    , ((0, xF86XK_MonBrightnessUp), spawn "sudo /usr/local/sbin/backlight up")
    , ((0, xF86XK_MonBrightnessDown), spawn "sudo /usr/local/sbin/backlight down")
    , ((0, xF86XK_Launch1), spawn "sudo /usr/local/sbin/backlight toggle")
    , ((0, xF86XK_Launch3), spawn "/usr/local/sbin/cpufreq_toggle_osd")
    ]

myKeys conf@(XConfig {XMonad.modMask = modm}) = M.fromList
             [
                ((m .|. modm, k), windows $ f i)
               | (i, k) <- zip (XMonad.workspaces conf) [xK_1 ..]
               , (f, m) <- [(W.view, 0), (W.shift, shiftMask), (copy, shiftMask .|. controlMask)]
             ]

myManageHook = composeAll
   [ className =? "Thunderbird" --> doShift "6m"
   , className =? "Keepassx"      --> doShift "7p"
   , className =? "Emacs"  --> doShift "4d"
   , manageDocks
   ]

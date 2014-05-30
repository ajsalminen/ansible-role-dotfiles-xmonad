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
    , workspaces = ["Aw", "Sw", "Dw" , "Fd", "Gd", "Hm", "Jp", "K", "L", "Ö", "Ä"]
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
               | (i, k) <- zip (XMonad.workspaces conf) workspaceKeys
               , (f, m) <- [(W.view, 0), (W.shift, shiftMask), (copy, shiftMask .|. controlMask)]
             ]
             where workspaceKeys = [xK_a, xK_s, xK_d, xK_f, xK_g, xK_h, xK_j, xK_k, xK_l, xK_odiaeresis, xK_adiaeresis]

myManageHook = composeAll
   [ className =? "Thunderbird" --> doShift "Hm"
   , className =? "Keepassx"      --> doShift "Jp"
   , className =? "Emacs"  --> doShift "Fd"
   , manageDocks
   ]

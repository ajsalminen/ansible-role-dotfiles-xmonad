module Main (main) where

import XMonad
import XMonad.Actions.UpdatePointer

import qualified Data.Map as M
import Graphics.X11.ExtraTypes.XF86
import XMonad.Hooks.ManageHelpers (isFullscreen, doFullFloat)

-- needed by CopyWindows bindings
import qualified XMonad.StackSet as W
import XMonad.Actions.CopyWindow

-- XMobar
import XMonad.Util.Run(spawnPipe)
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.DynamicLog
import System.IO
import XMonad.Util.EZConfig(additionalKeys)

import XMonad.Util.Paste


-- https://github.com/fancypantalons/XMonad-Config/blob/master/xmonad.hs
roleName :: Query String
roleName = stringProperty "WM_WINDOW_ROLE"

ctrlBackSpace w =
  let translatedProgs = ["Terminator"] in do
    c <- runQuery className w
    let toTranslate = any (== c) translatedProgs
    -- Unfortunately pasteSelection only supports ASCII
    -- If we simply use xdotool to send a middle click, it doens't always work depending on the position of the mouse pointer
    if toTranslate then sendKey controlMask xK_w else sendKey controlMask xK_BackSpace


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
                         >> updatePointer (Relative 0.99 0)
    } `additionalKeys`
    [ ((mod4Mask .|. shiftMask, xK_z), spawn "xscreensaver-command -lock")
    , ((mod4Mask, xK_b), sendMessage ToggleStruts)
    , ((mod4Mask, xK_n), spawn "~/bin/pomodoro-stop")
    , ((mod4Mask, xK_v), spawn "~/bin/pomodoro-interrupt")
    , ((0, xF86XK_MonBrightnessUp), spawn "sudo /usr/local/sbin/backlight up")
    , ((0, xF86XK_MonBrightnessDown), spawn "sudo /usr/local/sbin/backlight down")
    , ((0, xF86XK_Launch1), spawn "sudo /usr/local/sbin/backlight toggle")
    , ((0, xF86XK_Launch3), spawn "/usr/local/sbin/cpufreq_toggle_osd")
    , ((mod4Mask, xK_y     ), sendMessage Shrink)
    , ((mod4Mask, xK_o     ), sendMessage Expand)
    , ((mod4Mask, xK_u     ), windows W.focusDown  )
    , ((mod4Mask, xK_i     ), windows W.focusUp  )
    , ((mod3Mask, xK_e     ), withFocused ctrlBackSpace  )
    , ((mod3Mask, xK_r     ), sendKey controlMask xK_Delete  )
    , ((mod3Mask, xK_u     ), sendKey controlMask xK_Left  )
    , ((mod3Mask, xK_o     ), sendKey controlMask xK_Right  )
    , ((mod3Mask .|. mod5Mask, xK_u     ), sendKey controlMask xK_Up  )
    , ((mod3Mask .|. mod5Mask, xK_o     ), sendKey controlMask xK_Down  )
    , ((mod3Mask, xK_n     ), sendKey controlMask xK_Left  )
    , ((mod3Mask, xK_n     ), sendKey controlMask xK_Home  )
    , ((mod3Mask .|. mod5Mask, xK_n     ), sendKey controlMask xK_End  )
    , ((mod3Mask, xK_period     ), sendKey controlMask xK_x  )
    , ((mod3Mask, xK_semicolon     ), sendKey controlMask xK_c  )
    , ((mod3Mask, xK_odiaeresis     ), sendKey controlMask xK_v  )
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
   , title =? "password-private"      --> doShift "Jp"
   , title =? "password-kifi"      --> doShift "Jp"
   , roleName =? "terminal"      --> doShift "Gd"
   , className =? "Emacs"  --> doShift "Fd"
   , manageDocks
   ]
   <+> (isFullscreen --> doFullFloat)

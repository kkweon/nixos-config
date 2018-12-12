import Graphics.X11.ExtraTypes.XF86

import XMonad
import XMonad.Actions.Volume
import XMonad.Config.Desktop (desktopConfig)
import XMonad.Hooks.DynamicLog
  ( dynamicLogWithPP
  , dzen
  , ppOutput
  , ppTitle
  , xmobarPP
  , xmobarColor
  , shorten
  )
import XMonad.Hooks.ManageDocks
  ( avoidStruts
  , docks
  , docksEventHook
  , manageDocks
  )
import XMonad.Util.Dzen ((>=>), addArgs, center, dzenConfig, font, onCurr)
import XMonad.Util.EZConfig (additionalKeys)
import XMonad.Util.Run (hPutStrLn, spawnPipe)

main :: IO ()
main = do
  xmproc <- spawnPipe "xmobar"
  xmonad
    myConfig
      { logHook =
          dynamicLogWithPP
            xmobarPP
              { ppOutput = hPutStrLn xmproc
              , ppTitle = xmobarColor "green" "" . shorten 100
              }
      }

alert :: (Show a) => a -> X ()
alert = dzenConfig centered . show

alertDecimal :: Double -> X ()
alertDecimal = alert . round

centered =
  onCurr (center 150 100) >=>
  font "-*-arial-*-r-*-*-64-*-*-*-*-*-*-*" >=>
  addArgs ["-fg", "#80c0ff"] >=> addArgs ["-bg", "#000040"]

myConfig =
  def
    { terminal = "alacritty"
    , handleEventHook = docksEventHook <+> handleEventHook desktopConfig
    , manageHook = manageDocks <+> manageHook desktopConfig
    , layoutHook = avoidStruts $ layoutHook desktopConfig
    } `additionalKeys`
  [ ((0, xF86XK_AudioLowerVolume), lowerVolume 10 >>= alertDecimal)
  , ((0, xF86XK_AudioRaiseVolume), raiseVolume 10 >>= alertDecimal)
  , ((0, xF86XK_AudioMute), toggleMute >>= alert)
  , ((mod1Mask .|. controlMask, xK_l), spawn "slock") -- CTRL + ALT + L --> slock
  ]

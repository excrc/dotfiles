import           Data.List (concat)
import           Xmobar

main :: IO ()
main = xmobar config

fc :: String -> String -> String
fc c t = concat ["<fc=", c, ">", t, "</fc>"]

fn :: String -> String -> String
fn f t = concat ["<fn=", f, ">", t, "</fn>"]

black       = "#000004"
darkblue    = "#020221"

darkred     = "#be5046"
red         = "#e06c75"

darkgreen   = "#718e3f"
green       = "#b1bf75"

darkyellow  = "#d19a66"
yellow      = "#e5c07b"

violet      = "#635196"
blue        = "#99a4bc"

darkorange  = "#ff761a"
orange      = "#ffb07b"

darkcyan    = "#34bfa4"
cyan        = "#8bccbf"

lightgray   = "#b4b4b9"
white       = "#f8f8ff"

fg          = "#dfdfe5"
bg          = "#28282d"

myFont = "xft:monospace:size=11:antialias=true"

sep :: String
sep = " | "

config :: Config
config = defaultConfig
  { font = myFont
  , bgColor = bg
  , fgColor = fg
  , position = Top
  , sepChar =  "%"
  , alignSep = "}{"
  , template = concat
      [ " %UnsafeStdinReader%"
      , "}{"
      , "%battery%", sep
      , "%kbd%", sep
      , "%date%", sep
      , "%time%", " "
      ]
  , lowerOnStart =     True
  , hideOnStart =      False
  , allDesktops =      True
  , overrideRedirect = True
  , pickBroadest =     False
  , persistent =       True
  , iconRoot = "/home/ecquo/.config/xmobar/icons"
  , commands =
      [ Run UnsafeStdinReader
      , Run $ BatteryP
        ["BAT0"]
        ["-t", "<acstatus> (<left>%)"
        , "-L", "10"
        , "-H", "80"
        , "-p", "3"
        , "--"
        , "-O", "<fc=#ff00ff>On</fc>"
        , "-i", ""
        , "-L", "-15"
        , "-H", "-5"
        , "-l", "red"
        , "-m", "blue"
        , "-h", green
        , "-a", "notify-send -u critical 'Battery running out!!'"
        , "-A", "3"
        ] 600
      , Run $ Date "%a %d %b" "date" 36000
      , Run $ Date "%T" "time" 10
      , Run $ Kbd
          [ ("us(dvorak)" , "ENG")
          , ("ru"         , "RUS")
          , ("ua"         , "UKR")
          ]
      ]
  }

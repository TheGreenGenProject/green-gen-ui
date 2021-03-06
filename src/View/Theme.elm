module View.Theme exposing (..)


import Element exposing (rgb255, Color)

type alias Theme = {
    appForeground: Color,
    appBackground: Color,
    appTitleForeground: Color,
    background: Color,
    foreground: Color,
    tabForeground: Color,
    disabledTabForeground: Color,
    linkForeground: Color,
    userLinkForeground: Color,
    hashtagForeground: Color,
    textFieldForeground: Color,
    textFieldBackground: Color,
    textFieldPlaceHolder: Color,
    enabledButton: Color,
    disabledButton: Color,
    alertColor: Color,
    partnerForeground: Color,
    partnerBackground: Color,
    progressColor: Color,
    remainingProgressColor: Color,
    errorForeground: Color,
    flaggedForeground: Color,
    disabled: Color
 }

-- Dark mode theme
darkModeTheme = {
    appForeground          = white,
    appBackground          = darkCharcoal,
    appTitleForeground     = darkOrange,
    background             = lightCharcoal,
    foreground             = darkCharcoal,
    tabForeground          = darkOrange,
    disabledTabForeground  = lightCharcoal,
    linkForeground         = lightBlue,
    userLinkForeground     = lightBlue,
    hashtagForeground      = lightPurple,
    textFieldForeground    = lightBlue,
    textFieldBackground    = darkCharcoal,
    textFieldPlaceHolder   = lightBlue,
    enabledButton          = darkOrange,
    disabledButton         = grey,
    alertColor             = darkOrange,
    partnerForeground      = darkCharcoal,
    partnerBackground      = lightOrange,
    progressColor          = darkOrange,
    remainingProgressColor = lightCharcoal,
    errorForeground        = rgb255 164 25 0,
    flaggedForeground      = darkGrey,
    disabled               = rgb255 85 87 83
 }


--{-- App color scheme --}
--appForeground          = white
--appBackground          = darkCharcoal
--appTitleForeground     = darkOrange
--background             = lightCharcoal
--foreground             = darkCharcoal
--tabForeground          = darkOrange
--disabledTabForeground  = lightCharcoal
--linkForeground         = lightBlue
--userLinkForeground     = lightBlue
--hashtagForeground      = lightPurple
--textFieldForeground    = lightBlue
--textFieldBackground    = darkCharcoal
--enabledButton          = darkOrange
--disabledButton         = grey
--alertColor             = darkOrange
--partnerForeground      = darkCharcoal
--partnerBackground      = lightOrange
--progressColor          = darkOrange
--remainingProgressColor = lightCharcoal
--errorForeground = rgb255 164 25 0
--disabled        = rgb255 85 87 83
--background      = rgb255 50 170 110
--foreground      = rgb255 255 255 255

{-- Standard colors --}
lightRed        = rgb255 239 41 41
red             = rgb255 204 0 0
darkRed         = rgb255 164 25 0
lightOrange     = rgb255 252 175 62
orange          = rgb255 245 121 0
darkOrange      = rgb255 206 92 0
lightYellow     = rgb255 255 233 79
lighterYellow   = rgb255 255 245 150
yellow          = rgb255 237 212 0
darkYellow      = rgb255 196 160 0
lightGreen      = rgb255 138 226 52
green           = rgb255 115 210 22
darkGreen       = rgb255 78 154 6
lightBlue       = rgb255 114 159 207
blue            = rgb255 52 101 164
darkBlue        = rgb255 32 74 135
lightPurple     = rgb255 173 127 168
purple          = rgb255 117 80 123
darkPurple      = rgb255 92 53 102
lightBrown      = rgb255 233 185 110
brown           = rgb255 193 125 17
darkBrown       = rgb255 143 89 2
black           = rgb255 0 0 0
white           = rgb255 255 255 255
lightGrey       = rgb255 238 238 236
grey            = rgb255 211 215 207
darkGrey        = rgb255 186 189 182
lightGray       = rgb255 238 238 236
gray            = rgb255 211 215 207
darkGray        = rgb255 186 189 182
lightCharcoal   = rgb255 136 138 133
charcoal        = rgb255 85 87 83
darkCharcoal    = rgb255 46 52 54

module View.UIStyle exposing (..)

import Element exposing (Device, DeviceClass(..))
import View.Theme as Theme exposing (Theme)

type alias UIStyle = {
    defaultFontSize: Int,
    theme: Theme,
    -- Icons
    tiny: (Int, Int),
    small: (Int, Int),
    normal: (Int, Int),
    large: (Int, Int),
    extraLarge: (Int, Int)
 }

uiStyleFor: Device -> UIStyle
uiStyleFor device = case device.class of
    Desktop    -> desktop
    BigDesktop -> desktop
    Tablet     -> mobile
    Phone      -> mobile

isMobile: Device -> Bool
isMobile device = case device.class of
    Desktop    -> False
    BigDesktop -> False
    Tablet     -> False
    Phone      -> True

desktop = {
    defaultFontSize = 10,
    theme = Theme.darkModeTheme,
    -- Icons
    tiny       = (12, 12),
    small      = (16, 16),
    normal     = (24, 24),
    large      = (48, 48),
    extraLarge = (96, 96)
 }

mobile = {
    defaultFontSize = 9,
    theme = Theme.darkModeTheme,
    -- Icons
    tiny       = (12, 12),
    small      = (16, 16),
    normal     = (24, 24),
    large      = (48, 48),
    extraLarge = (96, 96)
 }
module Styles exposing (backgroundColor, inactiveColor, activeColor, headerColor, bold, font)

import Element as El
import Element.Font as Font

backgroundColor : El.Color
backgroundColor =
    El.rgb255 7 11 31

inactiveColor : El.Color
inactiveColor =
    El.rgb255 112 130 255

activeColor : El.Color
activeColor =
    El.rgb255 52 218 202

headerColor : El.Color
headerColor =
    El.rgb255 255 255 255

bold : El.Attribute msg
bold = Font.extraBold

font : El.Attribute msg
font = Font.family 
    [ Font.typeface "Source Sans Pro"
    , Font.typeface "Tahoma"
    , Font.typeface "Helvetica"
    , Font.typeface "Arial"
    , Font.sansSerif
    ]
module Colors exposing (..)

{-| This module aims to create color palettes that fit the Noordstar color scheme.
-}

import Color exposing (rgb255)
import Element
import Element.Background
import Element.Border
import Element.Font
import Svg
import Svg.Attributes exposing (fill)
import Widget.Material exposing (Palette)


type alias Color =
    Color.Color


type alias AllColors a =
    AllBlindnesses (AllModes (AllShades (AllNames a)))


allColors : AllColors Color
allColors =
    allBlindnesses
        (\blindness ->
            allModes
                (\mode ->
                    allShades
                        (\shade ->
                            allNames
                                (\name ->
                                    get blindness mode shade name
                                )
                        )
                )
        )


stdPicker : Picker
stdPicker =
    allColors.trichromatic.lightMode


type Name
    = Primary
    | Secondary
    | Tertiary
    | Quaternary
    | Extra
    | Black
    | White


type alias AllNames a =
    { primary : a
    , secondary : a
    , tertiary : a
    , quaternary : a
    , extra : a
    , black : a
    , white : a
    }


allNames : (Name -> a) -> AllNames a
allNames builder =
    { primary = builder Primary
    , secondary = builder Secondary
    , tertiary = builder Tertiary
    , quaternary = builder Quaternary
    , extra = builder Extra
    , black = builder Black
    , white = builder White
    }


type Shade
    = Light
    | Medium
    | Dark


type alias AllShades a =
    { light : a
    , medium : a
    , dark : a
    }


allShades : (Shade -> a) -> AllShades a
allShades builder =
    { light = builder Light
    , medium = builder Medium
    , dark = builder Dark
    }


{-| Based on the user's preferences, the website can be displayed in light mode or dark mode.
-}
type Mode
    = LightMode
    | DarkMode


type alias AllModes a =
    { lightMode : a
    , darkMode : a
    }


allModes : (Mode -> a) -> AllModes a
allModes builder =
    { lightMode = builder LightMode
    , darkMode = builder DarkMode
    }


{-| The website supports color blindness friendly color palettes.
This way, everyone can enjoy the website's graphs without having to distinguish
colors that they cannot distinguish.
-}
type Blindness
    = Trichromatic -- ALL THREE
    | Protanomaly -- BARELY RED
    | Deuteranomaly -- BARELY GREEN
    | Tritanomaly -- BARELY BLUE
    | Protanopia -- NO RED
    | Deuteranopia -- NO GREEN
    | Tritanopia -- NO BLUE
    | Monochromacy -- NO COLOR
    | BlueConeMonochromacy -- BARELY COLOR


type alias AllBlindnesses a =
    { trichromatic : a
    , protanomaly : a
    , deuteranomaly : a
    , tritanomaly : a
    , protanopia : a
    , deuteranopia : a
    , tritanopia : a
    , monochromacy : a
    , blueConeMonochromacy : a
    }


allBlindnesses : (Blindness -> a) -> AllBlindnesses a
allBlindnesses builder =
    { trichromatic = builder Trichromatic
    , protanomaly = builder Protanomaly
    , deuteranomaly = builder Deuteranomaly
    , tritanomaly = builder Tritanomaly
    , protanopia = builder Protanopia
    , deuteranopia = builder Deuteranopia
    , tritanopia = builder Tritanopia
    , monochromacy = builder Monochromacy
    , blueConeMonochromacy = builder BlueConeMonochromacy
    }


type alias Picker =
    AllShades (AllNames Color)


{-| Get a color based on the right criteria.
-}
get : Blindness -> Mode -> Shade -> Name -> Color
get blindness mode shade name =
    let
        trueName : Name
        trueName =
            name |> flipName mode

        trueShade : Shade
        trueShade =
            shade |> flipShade mode
    in
    toBlindnessPalette blindness
        |> toColor blindness trueName
        |> fromShade trueShade
        |> (\( r, g, b ) -> rgb255 r g b)


defaultPalette : Picker -> Palette
defaultPalette p =
    { primary = p.medium.primary
    , secondary = p.medium.secondary
    , background = p.dark.white
    , surface = p.light.white
    , error = p.light.secondary
    , on =
        { primary = p.light.white
        , secondary = p.light.white
        , background = p.light.black
        , surface = p.dark.black
        , error = p.medium.white
        }
    }


{-| Get a blindness color palette based on a blindness input.
-}
toBlindnessPalette : Blindness -> BlindnessPalette
toBlindnessPalette blindness =
    case blindness of
        Trichromatic ->
            trichromatic

        Protanomaly ->
            protanomaly

        Deuteranomaly ->
            deuteranomaly

        Tritanomaly ->
            tritanomaly

        Protanopia ->
            protanopia

        Deuteranopia ->
            deuteranopia

        Tritanopia ->
            tritanopia

        Monochromacy ->
            monochromacy

        BlueConeMonochromacy ->
            blueConeMonochromacy


flipName : Mode -> Name -> Name
flipName mode name =
    case mode of
        LightMode ->
            name

        DarkMode ->
            case name of
                Black ->
                    White

                White ->
                    Black

                _ ->
                    name


flipShade : Mode -> Shade -> Shade
flipShade mode shade =
    case ( mode, shade ) of
        ( LightMode, _ ) ->
            shade

        ( DarkMode, Dark ) ->
            Light

        ( DarkMode, Medium ) ->
            Medium

        ( DarkMode, Light ) ->
            Dark


{-| We distringuish the following colours:

           | Protan | Deuter | Tritan | Mono   |

  - Blue | Blue | Blue | Blue | Blue |
  - Green | Green | Green | Green | XXXXXX | [Orange]
  - Yellow | Yellow | Yellow | XXXXXX | Yellow | [Orange]
  - Orange | XXXXXX | XXXXXX | Orange | Orange | [Green,Red]
  - Red | Red | Red | Red | Red |
  - Black | Black | Black | Black | Black |
  - White | White | White | White | White |

In other words:

Primary | Blue | Blue | Blue | Blue |
Secondary | Red | Red | Red | Red |
Tertiary | Yellow | Yellow | Orange | Yellow |
Quaternary | Green | Green | Green | Orange |
-----------|--------|--------|--------|--------|
Rest | Orange | Orange | Yellow | Green |

-}
toColor : Blindness -> Name -> (BlindnessPalette -> ColorPalette)
toColor blindness name =
    case name of
        Primary ->
            .blue

        Secondary ->
            .red

        Tertiary ->
            case blindness of
                Tritanopia ->
                    .orange

                _ ->
                    .yellow

        Quaternary ->
            case blindness of
                Monochromacy ->
                    .orange

                _ ->
                    .green

        Extra ->
            case blindness of
                Tritanopia ->
                    .yellow

                Monochromacy ->
                    .green

                _ ->
                    .orange

        Black ->
            .black

        White ->
            .white


fromShade : Shade -> ColorPalette -> ( Int, Int, Int )
fromShade shade =
    case shade of
        Light ->
            .light

        Medium ->
            .medium

        Dark ->
            .dark


type alias ColorPalette =
    { light : ( Int, Int, Int )
    , medium : ( Int, Int, Int )
    , dark : ( Int, Int, Int )
    }


type alias BlindnessPalette =
    { blue : ColorPalette
    , green : ColorPalette
    , yellow : ColorPalette
    , orange : ColorPalette
    , red : ColorPalette
    , black : ColorPalette
    , white : ColorPalette
    }


{-| No color blindness
-}
trichromatic : BlindnessPalette
trichromatic =
    { blue = { light = ( 0x42, 0x87, 0xFF ), medium = ( 0x42, 0x7F, 0xF0 ), dark = ( 0x00, 0x54, 0xBD ) }
    , green = { light = ( 0x86, 0xEA, 0xD1 ), medium = ( 0x5E, 0xA4, 0x93 ), dark = ( 0x3E, 0x6D, 0x62 ) }
    , yellow = { light = ( 0xFC, 0xF9, 0x2B ), medium = ( 0xD2, 0xD0, 0x24 ), dark = ( 0xAF, 0xAD, 0x1E ) }
    , orange = { light = ( 0xFF, 0xBB, 0x93 ), medium = ( 0xCC, 0x95, 0x75 ), dark = ( 0xA3, 0x77, 0x5E ) }
    , red = { light = ( 0xDC, 0x00, 0x00 ), medium = ( 0xB0, 0x00, 0x00 ), dark = ( 0x8C, 0x00, 0x00 ) }
    , black = { light = ( 0x2C, 0x2C, 0x48 ), medium = ( 0x1D, 0x1D, 0x30 ), dark = ( 0x00, 0x00, 0x00 ) }
    , white = { light = ( 0xFF, 0xFF, 0xFF ), medium = ( 0xFE, 0xFA, 0xF5 ), dark = ( 0xF2, 0xEF, 0xEA ) }
    }


{-| Weak red vision
-}
protanomaly : BlindnessPalette
protanomaly =
    { blue = { light = ( 0x49, 0x86, 0xFE ), medium = ( 0x46, 0x7E, 0xEF ), dark = ( 0x00, 0x55, 0xB9 ) }
    , green = { light = ( 0xBE, 0xDD, 0xCA ), medium = ( 0x85, 0x9B, 0x8E ), dark = ( 0x58, 0x67, 0x5F ) }
    , yellow = { light = ( 0xFE, 0xF4, 0x88 ), medium = ( 0xDD, 0xCC, 0x23 ), dark = ( 0xB8, 0xAA, 0x1D ) }
    , orange = { light = ( 0xE6, 0xC4, 0x97 ), medium = ( 0xB8, 0x9D, 0x78 ), dark = ( 0x93, 0x7D, 0x61 ) }
    , red = { light = ( 0x9F, 0x47, 0x12 ), medium = ( 0x7F, 0x39, 0x0F ), dark = ( 0x65, 0x2D, 0x0C ) }
    , black = { light = ( 0x27, 0x2D, 0x49 ), medium = ( 0x1A, 0x1E, 0x31 ), dark = ( 0x00, 0x00, 0x00 ) }
    , white = { light = ( 0xFF, 0xFF, 0xFF ), medium = ( 0xFF, 0xFA, 0xF6 ), dark = ( 0xF4, 0xEE, 0xEA ) }
    }


{-| Weak green vision
-}
deuteranomaly : BlindnessPalette
deuteranomaly =
    { blue = { light = ( 0x18, 0x8A, 0xFA ), medium = ( 0x18, 0x82, 0xEC ), dark = ( 0x00, 0x59, 0xA9 ) }
    , green = { light = ( 0xC6, 0xD9, 0xD5 ), medium = ( 0x8B, 0x98, 0x95 ), dark = ( 0x5C, 0x65, 0x64 ) }
    , yellow = { light = ( 0xFE, 0xF3, 0x9C ), medium = ( 0xEE, 0xC5, 0x2B ), dark = ( 0xC6, 0xA4, 0x23 ) }
    , orange = { light = ( 0xF5, 0xBF, 0x92 ), medium = ( 0xC4, 0x99, 0x74 ), dark = ( 0x9D, 0x7A, 0x5D ) }
    , red = { light = ( 0xA9, 0x43, 0x00 ), medium = ( 0x87, 0x36, 0x00 ), dark = ( 0x6C, 0x2B, 0x00 ) }
    , black = { light = ( 0x26, 0x2E, 0x48 ), medium = ( 0x19, 0x1E, 0x30 ), dark = ( 0x00, 0x00, 0x00 ) }
    , white = { light = ( 0xFF, 0xFF, 0xFF ), medium = ( 0xFE, 0xFA, 0xF5 ), dark = ( 0xFA, 0xEC, 0xEC ) }
    }


{-| Weak blue vision
-}
tritanomaly : BlindnessPalette
tritanomaly =
    { blue = { light = ( 0x18, 0x93, 0xC5 ), medium = ( 0x18, 0x8A, 0xBA ), dark = ( 0x00, 0x5E, 0x88 ) }
    , green = { light = ( 0x8C, 0xE7, 0xE9 ), medium = ( 0x62, 0xA2, 0xA4 ), dark = ( 0x41, 0x6B, 0x6D ) }
    , yellow = { light = ( 0xFE, 0xF1, 0xAC ), medium = ( 0xDA, 0xC7, 0x92 ), dark = ( 0xB6, 0xA5, 0x79 ) }
    , orange = { light = ( 0xFF, 0xB8, 0xB2 ), medium = ( 0xCE, 0x92, 0x8D ), dark = ( 0xA5, 0x74, 0x71 ) }
    , red = { light = ( 0xDB, 0x0D, 0x00 ), medium = ( 0xAF, 0x0A, 0x00 ), dark = ( 0x8B, 0x08, 0x00 ) }
    , black = { light = ( 0x29, 0x2F, 0x3B ), medium = ( 0x1B, 0x1F, 0x27 ), dark = ( 0x00, 0x00, 0x00 ) }
    , white = { light = ( 0xFF, 0xFF, 0xFF ), medium = ( 0xFD, 0xFA, 0xFB ), dark = ( 0xF4, 0xED, 0xF7 ) }
    }


{-| Red-blind vision
-}
protanopia : BlindnessPalette
protanopia =
    { blue = { light = ( 0x4D, 0x86, 0xFE ), medium = ( 0x48, 0x7E, 0xEF ), dark = ( 0x00, 0x56, 0xB6 ) }
    , green = { light = ( 0xDE, 0xD6, 0xC6 ), medium = ( 0x9C, 0x96, 0x8B ), dark = ( 0x67, 0x64, 0x5D ) }
    , yellow = { light = ( 0xFF, 0xF2, 0xBE ), medium = ( 0xE3, 0xCA, 0x22 ), dark = ( 0xBD, 0xA8, 0x1D ) }
    , orange = { light = ( 0xD8, 0xCA, 0x9A ), medium = ( 0xAD, 0xA1, 0x7A ), dark = ( 0x8A, 0x81, 0x62 ) }
    , red = { light = ( 0x7D, 0x6F, 0x1C ), medium = ( 0x64, 0x59, 0x17 ), dark = ( 0x4F, 0x47, 0x12 ) }
    , black = { light = ( 0x24, 0x2E, 0x4A ), medium = ( 0x18, 0x1E, 0x31 ), dark = ( 0x00, 0x00, 0x00 ) }
    , white = { light = ( 0xFF, 0xFF, 0xFF ), medium = ( 0xFF, 0xFA, 0xF6 ), dark = ( 0xF5, 0xEE, 0xEA ) }
    }


{-| Green-blind vision
-}
deuteranopia : BlindnessPalette
deuteranopia =
    { blue = { light = ( 0x00, 0x8C, 0xF8 ), medium = ( 0x00, 0x84, 0xEA ), dark = ( 0x00, 0x5B, 0x9D ) }
    , green = { light = ( 0xEB, 0xD0, 0xD7 ), medium = ( 0xA5, 0x92, 0x97 ), dark = ( 0x6D, 0x61, 0x65 ) }
    , yellow = { light = ( 0xFF, 0xEF, 0xDC ), medium = ( 0xFE, 0xBF, 0x2E ), dark = ( 0xD3, 0x9F, 0x26 ) }
    , orange = { light = ( 0xF0, 0xC2, 0x92 ), medium = ( 0xBF, 0x9B, 0x74 ), dark = ( 0x99, 0x7B, 0x5D ) }
    , red = { light = ( 0x8C, 0x69, 0x00 ), medium = ( 0x70, 0x54, 0x00 ), dark = ( 0x59, 0x43, 0x00 ) }
    , black = { light = ( 0x23, 0x2F, 0x47 ), medium = ( 0x16, 0x1F, 0x30 ), dark = ( 0x00, 0x00, 0x00 ) }
    , white = { light = ( 0xFF, 0xFF, 0xFF ), medium = ( 0xFF, 0xF9, 0xFA ), dark = ( 0xFF, 0xEA, 0xED ) }
    }


{-| Blue-blind vision
-}
tritanopia : BlindnessPalette
tritanopia =
    { blue = { light = ( 0x00, 0x99, 0xA4 ), medium = ( 0x00, 0x90, 0x9B ), dark = ( 0x00, 0x63, 0x69 ) }
    , green = { light = ( 0x90, 0xE5, 0xF7 ), medium = ( 0x65, 0xA0, 0xAD ), dark = ( 0x43, 0x6B, 0x73 ) }
    , yellow = { light = ( 0xFF, 0xED, 0xF6 ), medium = ( 0xDF, 0xC2, 0xD1 ), dark = ( 0xBA, 0xA1, 0xAE ) }
    , orange = { light = ( 0xFF, 0xB7, 0xC3 ), medium = ( 0xCF, 0x90, 0x9B ), dark = ( 0xA6, 0x73, 0x7C ) }
    , red = { light = ( 0xDA, 0x14, 0x00 ), medium = ( 0xAF, 0x10, 0x00 ), dark = ( 0x8B, 0x0D, 0x00 ) }
    , black = { light = ( 0x27, 0x30, 0x34 ), medium = ( 0x1A, 0x20, 0x22 ), dark = ( 0x00, 0x00, 0x00 ) }
    , white = { light = ( 0xFF, 0xFF, 0xFF ), medium = ( 0xFC, 0xFA, 0xFF ), dark = ( 0xF5, 0xEC, 0xFE ) }
    }


{-| Color-less vision
-}
monochromacy : BlindnessPalette
monochromacy =
    { blue = { light = ( 0x80, 0x80, 0x80 ), medium = ( 0x7A, 0x7A, 0x7A ), dark = ( 0x47, 0x47, 0x47 ) }
    , green = { light = ( 0xC9, 0xC9, 0xC9 ), medium = ( 0x8D, 0x8D, 0x8D ), dark = ( 0x5E, 0x5E, 0x5E ) }
    , yellow = { light = ( 0xE2, 0xE2, 0xE2 ), medium = ( 0xBD, 0xBD, 0xBD ), dark = ( 0x9D, 0x9D, 0x9D ) }
    , orange = { light = ( 0xCB, 0xCB, 0xCB ), medium = ( 0xA2, 0xA2, 0xA2 ), dark = ( 0x81, 0x81, 0x81 ) }
    , red = { light = ( 0x42, 0x42, 0x42 ), medium = ( 0x35, 0x35, 0x35 ), dark = ( 0x2A, 0x2A, 0x2A ) }
    , black = { light = ( 0x2F, 0x2F, 0x2F ), medium = ( 0x1F, 0x1F, 0x1F ), dark = ( 0x00, 0x00, 0x00 ) }
    , white = { light = ( 0xFF, 0xFF, 0xFF ), medium = ( 0xFB, 0xFB, 0xFB ), dark = ( 0xEF, 0xEF, 0xEF ) }
    }


{-| Blue cone monochromacy
-}
blueConeMonochromacy : BlindnessPalette
blueConeMonochromacy =
    { blue = { light = ( 0x69, 0x83, 0xAE ), medium = ( 0x66, 0x7C, 0xA5 ), dark = ( 0x2D, 0x4C, 0x72 ) }
    , green = { light = ( 0xB1, 0xD5, 0xCC ), medium = ( 0x7C, 0x95, 0x8F ), dark = ( 0x52, 0x63, 0x5F ) }
    , yellow = { light = ( 0xEB, 0xEA, 0x9F ), medium = ( 0xC5, 0xC4, 0x85 ), dark = ( 0xA4, 0xA3, 0x6F ) }
    , orange = { light = ( 0xDE, 0xC5, 0xB7 ), medium = ( 0xB1, 0x9D, 0x92 ), dark = ( 0x8D, 0x7D, 0x74 ) }
    , red = { light = ( 0x7A, 0x2A, 0x2A ), medium = ( 0x62, 0x22, 0x22 ), dark = ( 0x4E, 0x1B, 0x1B ) }
    , black = { light = ( 0x2E, 0x2E, 0x38 ), medium = ( 0x1E, 0x1E, 0x25 ), dark = ( 0x00, 0x00, 0x00 ) }
    , white = { light = ( 0xFF, 0xFF, 0xFF ), medium = ( 0xFC, 0xFB, 0xF9 ), dark = ( 0xF0, 0xEF, 0xED ) }
    }


svgFill : Color -> Svg.Attribute msg
svgFill =
    Color.toCssString >> fill


svgStroke : Color -> Svg.Attribute msg
svgStroke =
    Color.toCssString >> Svg.Attributes.stroke


font : Color -> Element.Attribute msg
font =
    Color.toRgba >> Element.fromRgb >> Element.Font.color


border : Color -> Element.Attribute msg
border =
    Color.toRgba >> Element.fromRgb >> Element.Border.color


background : Color -> Element.Attribute msg
background =
    Color.toRgba >> Element.fromRgb >> Element.Background.color


transparent : Color
transparent =
    Color.rgba 0 0 0 0

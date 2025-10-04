module Theme exposing (button, column, countryFlag, padding, primaryButton, row, spacing, toggle, wrappedRow)

import Data exposing (Country(..))
import Element exposing (Attribute, Element, rgb, text)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input


row : List (Attribute msg) -> List (Element msg) -> Element msg
row attrs =
    Element.row (spacing :: attrs)


column : List (Attribute msg) -> List (Element msg) -> Element msg
column attrs =
    Element.column (spacing :: attrs)


wrappedRow : List (Attribute msg) -> List (Element msg) -> Element msg
wrappedRow attrs =
    Element.wrappedRow (spacing :: attrs)


padding : Attribute msg
padding =
    Element.padding rhythm


spacing : Attribute msg
spacing =
    Element.spacing rhythm


rhythm : Int
rhythm =
    8


countryFlag : Country -> String
countryFlag country =
    case country of
        Andorra ->
            "🇦🇩"

        Albania ->
            "🇦🇱"

        Austria ->
            "🇦🇹"

        Belarus ->
            "🇧🇾"

        Belgium ->
            "🇧🇪"

        BosniaAndHerzegovina ->
            "🇧🇦"

        Bulgaria ->
            "🇧🇬"

        Croatia ->
            "🇭🇷"

        Cyprus ->
            "🇨🇾"

        CzechRepublic ->
            "🇨🇿"

        Denmark ->
            "🇩🇰"

        Estonia ->
            "🇪🇪"

        Finland ->
            "🇫🇮"

        France ->
            "🇫🇷"

        Germany ->
            "🇩🇪"

        Greece ->
            "🇬🇷"

        Hungary ->
            "🇭🇺"

        Iceland ->
            "🇮🇸"

        Ireland ->
            "🇮🇪"

        Italy ->
            "🇮🇹"

        Kosovo ->
            "🇽🇰"

        Latvia ->
            "🇱🇻"

        Liechtenstein ->
            "🇱🇮"

        Lithuania ->
            "🇱🇹"

        Luxembourg ->
            "🇱🇺"

        Malta ->
            "🇲🇹"

        Moldova ->
            "🇲🇩"

        Monaco ->
            "🇲🇨"

        Montenegro ->
            "🇲🇪"

        Netherlands ->
            "🇳🇱"

        NorthMacedonia ->
            "🇲🇰"

        Norway ->
            "🇳🇴"

        Poland ->
            "🇵🇱"

        Portugal ->
            "🇵🇹"

        Romania ->
            "🇷🇴"

        SanMarino ->
            "🇸🇲"

        Serbia ->
            "🇷🇸"

        Slovakia ->
            "🇸🇰"

        Slovenia ->
            "🇸🇮"

        Spain ->
            "🇪🇸"

        Sweden ->
            "🇸🇪"

        Switzerland ->
            "🇨🇭"

        Ukraine ->
            "🇺🇦"

        UnitedKingdom ->
            "🇬🇧"

        Vatican ->
            "🇻🇦"


button :
    List (Attribute msg)
    ->
        { label : String
        , onPress : Maybe msg
        }
    -> Element msg
button attrs config =
    Input.button
        (Border.width 1
            :: Font.center
            :: padding
            :: attrs
            ++ (case config.onPress of
                    Nothing ->
                        [ Background.color (rgb 0.7 0.7 0.7) ]

                    Just _ ->
                        []
               )
        )
        { label = text config.label
        , onPress = config.onPress
        }


primaryButton :
    List (Attribute msg)
    -> { label : String, onPress : Maybe msg }
    -> Element msg
primaryButton attrs config =
    button (Background.color (rgb 0.6 1 0.6) :: attrs) config


toggle :
    List (Attribute (Maybe v))
    ->
        { label : String
        , selected : Maybe v
        , value : v
        }
    -> Element (Maybe v)
toggle attrs config =
    let
        selected : Bool
        selected =
            config.selected == Just config.value
    in
    button
        (if selected then
            Background.color (rgb 0.3 0.3 1)
                :: Font.color (rgb 1 1 1)
                :: Border.color (rgb 0 0 0)
                :: attrs

         else
            attrs
        )
        { label = config.label
        , onPress =
            if selected then
                Just Nothing

            else
                Just (Just config.value)
        }

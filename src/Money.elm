module Money exposing (..)

import Quantity exposing (Quantity(..))


type alias Euros =
    Quantity Int Euro


type Euro
    = Euro


euros : number -> Quantity number Euro
euros =
    Quantity


inEuros : Quantity number Euro -> number
inEuros (Quantity q) =
    q


formatEuros : Euros -> String
formatEuros (Quantity q) =
    "€" ++ String.fromInt q

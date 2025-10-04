module Money exposing (Euro(..), Euros, euros, formatEuros)

import Quantity exposing (Quantity(..))


type alias Euros =
    Quantity Int Euro


type Euro
    = Euro


euros : number -> Quantity number Euro
euros =
    Quantity


formatEuros : Euros -> String
formatEuros (Quantity q) =
    "€" ++ String.fromInt q

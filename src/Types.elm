module Types exposing (Action(..))

import Data exposing (Country, Player)
import Money exposing (Euros)
import SeqDict exposing (SeqDict)


type Action
    = TravelTo Player Country
    | InvestIn Player Country Euros
    | Election Country (SeqDict Player Euros)

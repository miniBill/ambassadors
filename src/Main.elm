module Main exposing (Model, Msg(..), init, main, update, view)

import Browser
import Data exposing (Country(..), Player)
import Element exposing (Attribute, Element, alignBottom, alignTop, el, fill, height, text, width)
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Money exposing (Euros)
import Quantity
import SeqDict exposing (SeqDict)
import SeqSet exposing (SeqSet)
import Theme exposing (column, row, wrappedRow)
import Types exposing (Action(..))


type alias State =
    { players : SeqDict Player Euros
    , countries :
        SeqDict
            Country
            { rulingCoalition : SeqSet Player
            , investments : SeqDict Player Euros
            , votes : SeqDict Player Euros
            }
    }


type alias Model =
    { reverseHistory : List Action
    , focused : Maybe Int
    , travel : ( Maybe Player, Maybe Country )
    , election : ( Maybe Country, SeqDict Player Euros )
    , investment : ( Maybe Player, Maybe Country, Euros )
    }


type Msg
    = CommitAction Action
    | Undo
    | PrepareTravel ( Maybe Player, Maybe Country )
    | PrepareElection ( Maybe Country, SeqDict Player Euros )
    | PrepareInvestment ( Maybe Player, Maybe Country, Euros )


main : Program () Model Msg
main =
    Browser.sandbox
        { init = init
        , view = \model -> Element.layout [ Theme.padding ] (view model)
        , update = update
        }


init : Model
init =
    { reverseHistory = []
    , focused = Nothing
    , travel = ( Nothing, Nothing )
    , election = ( Nothing, SeqDict.empty )
    , investment = ( Nothing, Nothing, Quantity.zero )
    }


view : Model -> Element Msg
view model =
    column []
        [ viewNextAction model
        , viewHistory model.reverseHistory
        ]


viewHistory : List Action -> Element Msg
viewHistory reverseHistory =
    let
        ( actionViews, finalState ) =
            List.foldl
                (\action ( views, state ) ->
                    let
                        newState =
                            updateState action state
                    in
                    ( box [ width fill ] (viewAction action state) :: views, newState )
                )
                ( [], initialState )
                reverseHistory
    in
    column
        [ Border.width 1
        , Theme.padding
        ]
        (viewState finalState :: actionViews)


updateState : Action -> State -> State
updateState action state =
    case action of
        TravelTo _ _ ->
            Debug.todo "updateState - branch 'TravelTo _ _' not implemented"

        InvestIn _ _ _ ->
            Debug.todo "updateState - branch 'InvestIn _ _ _' not implemented"

        Election _ _ ->
            Debug.todo "updateState - branch 'Election _ _' not implemented"


viewAction : Action -> State -> Element msg
viewAction action state =
    case action of
        TravelTo _ _ ->
            text "TODO: viewAction 'TravelTo _ _' "

        InvestIn _ _ _ ->
            text "TODO: viewAction 'InvestIn _ _ _'"

        Election _ _ ->
            text "TODO: viewAction 'Election _ _' "


initialState : State
initialState =
    { players =
        Data.players
            |> List.map
                (\player -> ( player, Money.euros 500 ))
            |> SeqDict.fromList
    , countries =
        Data.countries
            |> List.map
                (\country ->
                    ( country
                    , { rulingCoalition = SeqSet.empty
                      , votes = SeqDict.empty
                      , investments = SeqDict.empty
                      }
                    )
                )
            |> SeqDict.fromList
    }


viewState : State -> Element msg
viewState state =
    text "TODO: viewState"


viewNextAction : Model -> Element Msg
viewNextAction model =
    wrappedRow []
        [ box [ alignTop, height fill ] <| viewTravel model.travel
        , box [ alignTop, height fill ] <| viewInvestment model.investment
        , box [ alignTop, height fill ] <| viewElection model.election
        ]


viewTravel : ( Maybe Player, Maybe Country ) -> Element Msg
viewTravel ( player, country ) =
    column [ height fill ]
        [ el [ Font.bold ] (text "Travel")
        , Element.map
            (\newPlayer -> PrepareTravel ( newPlayer, country ))
            (playerPicker player)
        , Element.map
            (\newCountry -> PrepareTravel ( player, newCountry ))
            (countryPicker country)
        , Theme.primaryButton
            [ width fill
            , alignBottom
            ]
            { label = "Travel"
            , onPress = Maybe.map2 (\p c -> CommitAction (TravelTo p c)) player country
            }
        ]


playerPicker : Maybe Player -> Element (Maybe Player)
playerPicker selected =
    Data.players
        |> List.map
            (\player ->
                Theme.toggle []
                    { label = Data.playerToString player
                    , selected = selected
                    , value = player
                    }
            )
        |> row []


countryPicker : Maybe Country -> Element (Maybe Country)
countryPicker selected =
    Data.countries
        |> List.map
            (\country ->
                Theme.toggle [ Font.size 30 ]
                    { label = Theme.countryFlag country
                    , selected = selected
                    , value = country
                    }
            )
        |> wrappedRow []


viewInvestment : ( Maybe Player, Maybe Country, Euros ) -> Element Msg
viewInvestment ( player, country, euros ) =
    column []
        [ el [ Font.bold ] (text "Invest")
        , Element.map
            (\newPlayer -> PrepareInvestment ( newPlayer, country, euros ))
            (playerPicker player)
        , Element.map
            (\newCountry -> PrepareInvestment ( player, newCountry, euros ))
            (countryPicker country)
        , Input.text [ width fill ]
            { label = Input.labelLeft [] (text "€")
            , text = String.fromInt (Money.inEuros euros)
            , onChange =
                \newEuros ->
                    PrepareInvestment
                        ( player
                        , country
                        , newEuros
                            |> String.toInt
                            |> Maybe.map Money.euros
                            |> Maybe.withDefault euros
                        )
            , placeholder = Nothing
            }
        , Theme.primaryButton
            [ width fill
            , alignBottom
            ]
            { label = "Invest"
            , onPress = Maybe.map2 (\p c -> CommitAction (InvestIn p c euros)) player country
            }
        ]


viewElection : ( Maybe Country, SeqDict Player Euros ) -> Element Msg
viewElection arg1 =
    text "TODO: viewElection"


box : List (Attribute msg) -> Element msg -> Element msg
box attrs child =
    el
        (Border.width 1
            :: Theme.padding
            :: attrs
        )
        child


update : Msg -> Model -> Model
update msg model =
    case msg of
        CommitAction action ->
            { model | reverseHistory = action :: model.reverseHistory }

        Undo ->
            { model | reverseHistory = List.drop 1 model.reverseHistory }

        PrepareTravel travel ->
            { model | travel = travel }

        PrepareElection election ->
            { model | election = election }

        PrepareInvestment investment ->
            { model | investment = investment }

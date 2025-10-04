module Main exposing (Model, Msg(..), init, main, update, view)

import Browser
import Data exposing (Country(..), Player)
import Element exposing (Attribute, Column, Element, alignBottom, alignRight, alignTop, centerY, el, fill, fillPortion, height, shrink, table, text, width)
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Maybe.Extra
import Money exposing (Euros)
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
    , country : Maybe Country
    , travel : Maybe Player
    , election : SeqDict Player String
    , investment : ( Maybe Player, String )
    }


type Msg
    = CommitAction Action
    | Undo
    | PrepareCountry (Maybe Country)
    | PrepareTravel (Maybe Player)
    | PrepareElection (SeqDict Player String)
    | PrepareInvestment (Maybe Player) String


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
    , country = Nothing
    , travel = Nothing
    , election = SeqDict.empty
    , investment = ( Nothing, "" )
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
    column
        [ Border.width 1
        , Theme.padding
        ]
        (Element.map PrepareCountry (countryPicker model.country)
            :: ([ viewTravel model.country model.travel
                , viewInvestment model.country model.investment
                , viewElection model.country model.election
                ]
                    |> List.map
                        (\children ->
                            row
                                [ Border.width 1
                                , Theme.padding
                                , width fill
                                ]
                                children
                        )
               )
        )


viewTravel : Maybe Country -> Maybe Player -> List (Element Msg)
viewTravel country player =
    [ Element.map PrepareTravel (playerPicker [] player)
    , Theme.primaryButton [ alignRight ]
        { label = "Travel"
        , onPress = Maybe.map2 (\p c -> CommitAction (TravelTo p c)) player country
        }
    ]


viewInvestment : Maybe Country -> ( Maybe Player, String ) -> List (Element Msg)
viewInvestment country ( player, euros ) =
    [ Element.map
        (\newPlayer -> PrepareInvestment newPlayer euros)
        (playerPicker [] player)
    , Input.text [ width fill ]
        { label = Input.labelLeft [] (text "€")
        , text = euros
        , onChange = PrepareInvestment player
        , placeholder = Nothing
        }
    , Theme.primaryButton [ alignRight ]
        { label = "Invest"
        , onPress =
            Maybe.map3
                (\p c e -> CommitAction (InvestIn p c (Money.euros e)))
                player
                country
                (String.toInt euros)
        }
    ]


viewElection : Maybe Country -> SeqDict Player String -> List (Element Msg)
viewElection country votes =
    let
        columns : List (Column Player Msg)
        columns =
            [ { header = Element.none
              , width = shrink
              , view = \player -> el [ centerY ] (text (Data.playerToString player))
              }
            , { header = Element.none
              , width = fill
              , view =
                    \player ->
                        let
                            previous : String
                            previous =
                                SeqDict.get player votes
                                    |> Maybe.withDefault ""
                        in
                        Input.text [ width fill ]
                            { label = Input.labelLeft [] (text "€")
                            , text = previous
                            , onChange =
                                \newEuros ->
                                    SeqDict.insert player newEuros votes
                                        |> PrepareElection
                            , placeholder = Nothing
                            }
              }
            ]

        button : Element Msg
        button =
            Theme.primaryButton [ alignRight ]
                { label = "Elect"
                , onPress = Maybe.map2 (\c v -> CommitAction (Election c v)) country parsedVotes
                }

        parsedVotes : Maybe (SeqDict Player Euros)
        parsedVotes =
            votes
                |> SeqDict.toList
                |> Maybe.Extra.combineMap
                    (\( k, v ) ->
                        Maybe.map
                            (\e ->
                                ( k
                                , Money.euros e
                                )
                            )
                            (String.toInt v)
                    )
                |> Maybe.map SeqDict.fromList
    in
    [ table [ Theme.spacing ]
        { data = Data.players
        , columns = columns
        }
    , button
    ]


playerPicker : List (Attribute (Maybe Player)) -> Maybe Player -> Element (Maybe Player)
playerPicker attrs selected =
    Data.players
        |> List.map
            (\player ->
                Theme.toggle []
                    { label = Data.playerToString player
                    , selected = selected
                    , value = player
                    }
            )
        |> row attrs


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

        PrepareCountry country ->
            { model | country = country }

        PrepareTravel travel ->
            { model | travel = travel }

        PrepareElection election ->
            { model | election = election }

        PrepareInvestment player euros ->
            { model | investment = ( player, euros ) }

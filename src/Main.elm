module Main exposing (Model, Msg(..), init, main, update, view)

import Browser
import Data exposing (Country(..), Player)
import Element exposing (Attribute, Column, Element, alignTop, centerY, el, fill, rgb, shrink, table, text, width)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Html exposing (Html)
import Html.Attributes
import Maybe.Extra
import Money exposing (Euros)
import Quantity
import SeqDict exposing (SeqDict)
import SeqSet exposing (SeqSet)
import Theme
import Types exposing (Action(..))


type alias State =
    { players : SeqDict Player Euros
    , countries : SeqDict Country CountryState
    }


type alias CountryState =
    { rulingCoalition : SeqSet Player
    , investments : SeqDict Player Euros
    , votes : SeqDict Player Euros
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
        , view = \model -> Element.layout [] (view model)
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
    Theme.column [ Theme.padding ]
        [ viewNextAction model
        , viewHistory model.reverseHistory
        ]


viewHistory : List Action -> Element Msg
viewHistory reverseHistory =
    let
        actionViews : List ( Element msg, State )
        actionViews =
            List.foldr
                (\action ( views, state ) ->
                    let
                        newState : State
                        newState =
                            updateState action state
                    in
                    ( ( viewAction action state, newState ) :: views
                    , newState
                    )
                )
                ( [], initialState )
                reverseHistory
                |> Tuple.first

        columns : List (Column ( Element msg, State ) msg)
        columns =
            [ { header = Element.none
              , width = shrink
              , view =
                    \( f, state ) ->
                        Theme.column []
                            [ el
                                [ Border.width 1
                                , Theme.padding
                                , width fill
                                , Background.color (rgb 0.9 0.9 0.6)
                                , alignTop
                                ]
                                f
                            , viewPlayersState state.players
                            ]
              }
            , { header = Element.none
              , width = fill
              , view = \( _, state ) -> viewCountryState state.countries
              }
            ]
    in
    table [ Theme.spacing ]
        { columns = columns
        , data = actionViews ++ [ ( text "Initial", initialState ) ]
        }


updateState : Action -> State -> State
updateState action state =
    case action of
        TravelTo player _ ->
            { state
                | players =
                    SeqDict.updateIfExists
                        player
                        (Quantity.plus (Money.euros 10))
                        state.players
            }

        InvestIn player country euros ->
            { state
                | players = SeqDict.updateIfExists player (Quantity.minus euros) state.players
                , countries =
                    SeqDict.update
                        country
                        (\countryState ->
                            let
                                old : CountryState
                                old =
                                    countryState
                                        |> Maybe.withDefault
                                            { rulingCoalition = SeqSet.empty
                                            , investments = SeqDict.empty
                                            , votes = SeqDict.empty
                                            }
                            in
                            { old
                                | investments =
                                    SeqDict.insert player
                                        (old.investments
                                            |> SeqDict.get player
                                            |> Maybe.withDefault Quantity.zero
                                            |> Quantity.plus euros
                                        )
                                        old.investments
                            }
                                |> Just
                        )
                        state.countries
            }

        Election _ _ ->
            Debug.todo "updateState - branch 'Election _ _' not implemented"


viewAction : Action -> State -> Element msg
viewAction action state =
    case action of
        TravelTo p c ->
            text ("🚄 " ++ Data.playerToString p ++ " ⇒ " ++ Theme.countryFlag c)

        InvestIn p c e ->
            text ("💰 " ++ Data.playerToString p ++ " " ++ Money.formatEuros e ++ " ⇒ " ++ Theme.countryFlag c)

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
        List.foldl
            (\player acc ->
                SeqDict.insert
                    (Data.initialCountry player)
                    { rulingCoalition = SeqSet.singleton player
                    , investments = SeqDict.empty
                    , votes = SeqDict.singleton player (Money.euros 100)
                    }
                    acc
            )
            SeqDict.empty
            Data.players
    }


viewPlayersState : SeqDict Player Euros -> Element msg
viewPlayersState players =
    let
        columns : List (Column () msg)
        columns =
            players
                |> SeqDict.toList
                |> List.map
                    (\( player, euros ) ->
                        { width = shrink
                        , view = \_ -> el [ Font.alignRight ] (text (Money.formatEuros euros))
                        , header = text (Data.playerToString player)
                        }
                    )
    in
    table
        [ Border.width 1
        , Theme.padding
        , Theme.spacing
        , width shrink
        ]
        { data = [ () ]
        , columns = columns
        }


viewCountryState : SeqDict Country CountryState -> Element msg
viewCountryState countries =
    let
        header : List (Html msg)
        header =
            [ Html.div [] []
            , Html.div
                [ Html.Attributes.colspan (List.length Data.players)
                , Html.Attributes.style "background-color" "#fdd"
                , Html.Attributes.style "padding" "8px"
                , Html.Attributes.style "grid-column-start" "2"
                , Html.Attributes.style "grid-column-end" (String.fromInt (2 + List.length Data.players))
                ]
                [ Html.text "Votes" ]
            , Html.div
                [ Html.Attributes.colspan (List.length Data.players)
                , Html.Attributes.style "background-color" "#ddf"
                , Html.Attributes.style "padding" "8px"
                , Html.Attributes.style "grid-column-start" (String.fromInt (2 + List.length Data.players))
                , Html.Attributes.style "grid-column-end" (String.fromInt (2 + 2 * List.length Data.players))
                ]
                [ Html.text "Investment" ]
            , Html.div [] []
            ]
                ++ headerNamesCells
                ++ headerNamesCells

        headerNamesCells : List (Html msg)
        headerNamesCells =
            List.map
                (\player ->
                    Html.div [ Html.Attributes.style "padding" "8px" ]
                        [ Html.text (Data.playerToString player) ]
                )
                Data.players

        rows : List (Html msg)
        rows =
            countries
                |> SeqDict.toList
                |> List.concatMap viewRow

        viewRow :
            ( Country, CountryState )
            -> List (Html msg)
        viewRow ( country, { rulingCoalition, investments, votes } ) =
            (Html.text (Theme.countryFlag country)
                :: List.map
                    (\player ->
                        SeqDict.get player votes
                            |> Maybe.map Money.formatEuros
                            |> Maybe.withDefault ""
                            |> Html.text
                    )
                    Data.players
                ++ List.map
                    (\player ->
                        SeqDict.get player investments
                            |> Maybe.map Money.formatEuros
                            |> Maybe.withDefault ""
                            |> Html.text
                    )
                    Data.players
            )
                |> List.map
                    (\e ->
                        Html.div
                            [ Html.Attributes.style "padding" "8px"
                            ]
                            [ e ]
                    )
    in
    (header ++ rows)
        |> Html.div
            [ Html.Attributes.style "display" "grid"
            , Html.Attributes.style "grid-template-columns"
                ("auto repeat(" ++ String.fromInt (2 * List.length Data.players) ++ ", 1fr)")
            , Html.Attributes.style "text-align" "center"
            ]
        |> Element.html
        |> el
            [ Border.width 1
            , Theme.padding
            , Theme.spacing
            ]


viewNextAction : Model -> Element Msg
viewNextAction model =
    let
        box : List (Element msg) -> Element msg
        box children =
            Theme.column
                [ Border.width 1
                , Theme.padding
                , width fill
                ]
                children
    in
    box
        [ Element.map PrepareCountry (countryPicker model.country)
        , Theme.row [ width fill ]
            [ Theme.column
                [ width fill
                , alignTop
                ]
                [ box (viewTravel model.country model.travel)
                , box (viewInvestment model.country model.investment)
                ]
            , Theme.column
                [ Border.width 1
                , Theme.padding
                , width fill
                ]
                (viewElection model.country model.election)
            ]
        ]


viewTravel : Maybe Country -> Maybe Player -> List (Element Msg)
viewTravel country player =
    [ Element.map PrepareTravel (playerPicker [] player)
    , Theme.primaryButton [ width fill ]
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
    , Theme.primaryButton [ width fill ]
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
            Theme.primaryButton [ width fill ]
                { label = "Elect"
                , onPress = Maybe.map2 (\c v -> CommitAction (Election c v)) country parsedVotes
                }

        parsedVotes : Maybe (SeqDict Player Euros)
        parsedVotes =
            Data.players
                |> Maybe.Extra.combineMap
                    (\player ->
                        SeqDict.get player votes
                            |> Maybe.andThen String.toInt
                            |> Maybe.map
                                (\e ->
                                    ( player
                                    , Money.euros e
                                    )
                                )
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
        |> Theme.row attrs


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
        |> Theme.wrappedRow []


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

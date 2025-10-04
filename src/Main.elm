module Main exposing (Model, Msg(..), main)

import Browser
import Data exposing (Country, Player)
import Element exposing (Attribute, Color, Column, Element, IndexedColumn, alignRight, alignTop, centerX, centerY, el, fill, indexedTable, rgb, rgb255, shrink, table, text, width)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Html exposing (Html)
import Html.Attributes
import Maybe.Extra
import Money exposing (Euros)
import Quantity
import Round
import SeqDict exposing (SeqDict)
import SeqSet exposing (SeqSet)
import Theme exposing (column)
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
    , election : ( SeqDict Player String, SeqSet Player )
    , investment : ( Maybe Player, String )
    }


type Msg
    = CommitAction Action
    | Undo
    | PrepareCountry (Maybe Country)
    | PrepareTravel (Maybe Player)
    | PrepareElection (SeqDict Player String) (SeqSet Player)
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
    , election = ( SeqDict.empty, SeqSet.empty )
    , investment = ( Nothing, "" )
    }


view : Model -> Element Msg
view model =
    let
        ( historyView, finalState ) =
            viewHistory model.reverseHistory
    in
    Theme.column [ Theme.padding ]
        [ viewNextAction model finalState
        , historyView
        ]


viewHistory : List Action -> ( Element Msg, State )
viewHistory reverseHistory =
    let
        ( actionViews, finalState ) =
            List.foldr
                (\action ( views, state ) ->
                    let
                        newState : State
                        newState =
                            updateState action state
                    in
                    ( ( viewAction action, newState ) :: views
                    , newState
                    )
                )
                ( [], initialState )
                reverseHistory

        columns : List (IndexedColumn ( ( Color, Element Msg ), State ) Msg)
        columns =
            [ { header = Element.none
              , width = shrink
              , view =
                    \i ( ( color, f ), state ) ->
                        Theme.column []
                            [ el
                                [ Border.width 1
                                , Theme.padding
                                , width fill
                                , Background.color color
                                , alignTop
                                ]
                                f
                            , viewPlayersState state
                            , if i == 0 && not (List.isEmpty reverseHistory) then
                                Theme.button
                                    [ width fill
                                    , Background.color (rgb255 255 0 0)
                                    ]
                                    { label = "Undo"
                                    , onPress = Just Undo
                                    }

                              else
                                Element.none
                            ]
              }
            , { header = Element.none
              , width = fill
              , view = \_ ( _, state ) -> viewCountryState state.countries
              }
            ]
    in
    ( indexedTable [ Theme.spacing ]
        { columns = columns
        , data = actionViews ++ [ ( ( rgb 1 1 1, el [ centerX ] (text "Initial") ), initialState ) ]
        }
    , finalState
    )


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

        Election country votes coalition ->
            let
                totalInvestment : Euros
                totalInvestment =
                    old.investments
                        |> SeqDict.values
                        |> Quantity.sum
                        |> Quantity.plus (initialInvestment country)

                old : CountryState
                old =
                    SeqDict.get country state.countries
                        |> Maybe.withDefault
                            { rulingCoalition = SeqSet.empty
                            , investments = SeqDict.empty
                            , votes = SeqDict.empty
                            }
            in
            { state
                | players =
                    SeqDict.foldl
                        (\player vote ->
                            let
                                rent : Euros
                                rent =
                                    Money.euros
                                        (100
                                            * Money.inEuros
                                                (Maybe.withDefault Quantity.zero <|
                                                    SeqDict.get player old.investments
                                                )
                                            // Money.inEuros totalInvestment
                                        )
                            in
                            SeqDict.updateIfExists player
                                (\initial ->
                                    initial
                                        -- €100 per vote
                                        |> Quantity.plus (Money.euros 100)
                                        -- Eat the rich
                                        |> Quantity.minus (eatTheRich initial state)
                                        |> Quantity.plus rent
                                        |> Quantity.minus vote
                                )
                        )
                        state.players
                        votes
                , countries =
                    SeqDict.insert
                        country
                        { old
                            | rulingCoalition = coalition
                            , votes =
                                SeqDict.foldl
                                    (\player vote acc ->
                                        if vote == Quantity.zero then
                                            acc

                                        else
                                            SeqDict.insert player
                                                (SeqDict.get player acc
                                                    |> Maybe.withDefault Quantity.zero
                                                    |> Quantity.plus vote
                                                )
                                                acc
                                    )
                                    old.votes
                                    votes
                        }
                        state.countries
            }


totalLiquidity : State -> Euros
totalLiquidity state =
    state.players
        |> SeqDict.values
        |> Quantity.sum


eatTheRich : Euros -> State -> Euros
eatTheRich initial state =
    Money.euros
        (100
            * Money.inEuros initial
            // Money.inEuros (totalLiquidity state)
        )


viewAction : Action -> ( Color, Element msg )
viewAction action =
    case action of
        TravelTo player country ->
            ( rgb255 255 178 178
            , column [ width fill ]
                [ el [ centerX ] (text ("🚄 ⇒ " ++ Theme.countryFlag country))
                , el [ centerX ] (text (Data.playerToString player))
                ]
            )

        InvestIn player country euros ->
            ( rgb255 178 255 178
            , column [ width fill ]
                [ el [ centerX ] (text ("💰 ⇒ " ++ Theme.countryFlag country))
                , el [ centerX ] (text (Data.playerToString player ++ " " ++ Money.formatEuros euros))
                ]
            )

        Election country _ coalition ->
            ( rgb255 178 178 255
            , column [ width fill ]
                [ el [ centerX ] (text ("🗳️ ⇒ " ++ Theme.countryFlag country))
                , el [ centerX ] (text (String.join ", " (List.map Data.playerToString (SeqSet.toList coalition))))
                ]
            )


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


viewPlayersState : State -> Element msg
viewPlayersState state =
    let
        columns : List (Column ( Player, Euros ) msg)
        columns =
            [ { width = shrink
              , view = \( player, _ ) -> text (Data.playerToString player)
              , header = Element.none
              }
            , { width = shrink
              , view = \( _, euros ) -> el [ Font.alignRight ] (text (Money.formatEuros euros))
              , header = Element.none
              }
            , { width = shrink
              , view = \( _, euros ) -> el [ Font.alignRight ] (text (Money.formatEuros (eatTheRich euros state)))
              , header = Element.none
              }
            ]
    in
    table
        [ Border.width 1
        , Theme.padding
        , Theme.spacing
        , width shrink
        ]
        { data = SeqDict.toList state.players
        , columns = columns
        }


viewCountryState : SeqDict Country CountryState -> Element msg
viewCountryState countries =
    let
        padding : String
        padding =
            "4px"

        cell : List (Html.Attribute msg) -> Html msg -> Html msg
        cell attrs e =
            Html.div
                (Html.Attributes.style "padding" padding :: attrs)
                [ e ]

        header : List (Html msg)
        header =
            [ Html.div
                [ Html.Attributes.style "grid-row" "span 2"
                ]
                []
            , Html.div
                [ Html.Attributes.style "padding" padding
                , Html.Attributes.style "grid-row" "span 2"
                ]
                [ Html.text "Ruling"
                , Html.br [] []
                , Html.text "Coalition"
                ]
            , cell
                [ Html.Attributes.colspan (List.length Data.players)
                , Html.Attributes.style "background-color" "rgb(178,178,255)"
                , Html.Attributes.style "grid-column" ("span " ++ String.fromInt (1 + 2 * List.length Data.players))
                ]
                (Html.text "Votes")
            , cell
                [ Html.Attributes.colspan (List.length Data.players)
                , Html.Attributes.style "background-color" "rgb(178,255,178)"
                , Html.Attributes.style "grid-column" ("span " ++ String.fromInt (2 + 2 * List.length Data.players))
                ]
                (Html.text "Investment")
            ]
                ++ headerNamesCells
                ++ cell [] (Html.text "Total")
                :: headerNamesCells
                ++ [ cell [] (Html.text "Initial")
                   , cell [] (Html.text "Total")
                   ]

        headerNamesCells : List (Html msg)
        headerNamesCells =
            List.map
                (\player ->
                    cell
                        [ Html.Attributes.style "grid-column" "span 2"
                        ]
                        (Html.text (Data.playerToString player))
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
            let
                euroCell : Euros -> Html msg
                euroCell e =
                    cell [ Html.Attributes.style "text-align" "right" ] (Html.text (Money.formatEuros e))

                totalVotes : Euros
                totalVotes =
                    votes
                        |> SeqDict.values
                        |> Quantity.sum

                totalInvestments : Euros
                totalInvestments =
                    investments
                        |> SeqDict.values
                        |> Quantity.sum
                        |> Quantity.plus (initialInvestment country)

                withPercentage : Maybe Euros -> Euros -> List (Html msg)
                withPercentage maybeMoney total =
                    case maybeMoney of
                        Nothing ->
                            [ Html.div [] [], Html.div [] [] ]

                        Just money ->
                            let
                                percentInt : Int
                                percentInt =
                                    100
                                        * Money.inEuros money
                                        // Money.inEuros total
                            in
                            [ euroCell money
                            , cell [ Html.Attributes.style "text-align" "right" ]
                                (Html.text ("(" ++ String.fromInt percentInt ++ "%)"))
                            ]
            in
            cell [] (Html.text (Theme.countryFlag country))
                :: cell [] (Html.text (String.join ", " (List.map Data.playerToString (SeqSet.toList rulingCoalition))))
                :: List.concatMap
                    (\player ->
                        withPercentage (SeqDict.get player votes) totalVotes
                    )
                    Data.players
                ++ euroCell totalVotes
                :: List.concatMap
                    (\player ->
                        withPercentage (SeqDict.get player investments) totalInvestments
                    )
                    Data.players
                ++ (let
                        initial : Euros
                        initial =
                            initialInvestment country
                    in
                    [ euroCell initial
                    , investments
                        |> SeqDict.values
                        |> (::) initial
                        |> Quantity.sum
                        |> euroCell
                    ]
                   )
    in
    (header ++ rows)
        |> Html.div
            [ Html.Attributes.style "display" "grid"
            , Html.Attributes.style "grid-template-columns"
                ("auto auto repeat(" ++ String.fromInt (3 + 4 * List.length Data.players) ++ ", 1fr)")
            , Html.Attributes.style "text-align" "center"
            , Html.Attributes.style "font-variant-numeric" "tabular-nums"
            ]
        |> Element.html
        |> el
            [ Border.width 1
            , Theme.padding
            , Theme.spacing
            ]


initialInvestment : Country -> Euros
initialInvestment country =
    Money.euros (Data.countryToPopulation country // 100000)


viewNextAction : Model -> State -> Element Msg
viewNextAction model finalState =
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
                (viewElection model.country model.election finalState)
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


viewElection : Maybe Country -> ( SeqDict Player String, SeqSet Player ) -> State -> List (Element Msg)
viewElection country ( votes, coalition ) finalState =
    let
        action : Maybe Action
        action =
            Maybe.map2 (\c v -> Election c v coalition) country parsedVotes

        nextState : Maybe CountryState
        nextState =
            Maybe.Extra.andThen2
                (\c a -> (updateState a finalState).countries |> SeqDict.get c)
                country
                action

        totalVotes : Maybe Euros
        totalVotes =
            nextState
                |> Maybe.map
                    (\s ->
                        s.votes
                            |> SeqDict.values
                            |> Quantity.sum
                    )

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
                                    PrepareElection (SeqDict.insert player newEuros votes) coalition
                            , placeholder = Nothing
                            }
              }
            , { header = Element.none
              , width = shrink
              , view =
                    \player ->
                        case
                            Maybe.map2 Tuple.pair
                                (Maybe.andThen
                                    (\s -> SeqDict.get player s.votes)
                                    nextState
                                )
                                totalVotes
                        of
                            Nothing ->
                                Element.none

                            Just ( v, t ) ->
                                el
                                    [ centerY
                                    , width fill
                                    , Font.alignRight
                                    ]
                                    (text
                                        (Round.round 1
                                            (100
                                                * toFloat (Money.inEuros v)
                                                / toFloat (Money.inEuros t)
                                            )
                                            ++ "%"
                                        )
                                    )
              }
            ]

        button : Element Msg
        button =
            Theme.primaryButton [ width fill ]
                { label = "Elect"
                , onPress = Maybe.map CommitAction action
                }

        parsedVotes : Maybe (SeqDict Player Euros)
        parsedVotes =
            Data.players
                |> Maybe.Extra.combineMap
                    (\player ->
                        case SeqDict.get player votes of
                            Nothing ->
                                Just ( player, Quantity.zero )

                            Just "" ->
                                Just ( player, Quantity.zero )

                            Just v ->
                                String.toInt v
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
    , Data.players
        |> List.map
            (\player ->
                Theme.button
                    (if SeqSet.member player coalition then
                        [ Background.color (rgb 0.3 0.3 1)
                        , Font.color (rgb 1 1 1)
                        , Border.color (rgb 0 0 0)
                        , alignRight
                        ]

                     else
                        [ alignRight ]
                    )
                    { label = Data.playerToString player
                    , onPress =
                        if SeqSet.member player coalition then
                            SeqSet.remove player coalition
                                |> Just

                        else
                            SeqSet.insert player coalition
                                |> Just
                    }
            )
        |> (::) (el [ Font.bold ] (text "Winning coalition"))
        |> Theme.row [ width fill ]
        |> Element.map (PrepareElection votes)
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

        PrepareElection election coalition ->
            { model | election = ( election, coalition ) }

        PrepareInvestment player euros ->
            { model | investment = ( player, euros ) }

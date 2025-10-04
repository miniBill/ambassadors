module Main exposing (Model, Msg(..), init, main, update, view)

import Browser
import Data exposing (Country(..), Player)
import Element exposing (Attribute, Element, alignBottom, alignTop, el, fill, fillPortion, height, text, width)
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
    , country : Maybe Country
    , travel : Maybe Player
    , election : SeqDict Player Euros
    , investment : ( Maybe Player, Euros )
    }


type Msg
    = CommitAction Action
    | Undo
    | PrepareCountry (Maybe Country)
    | PrepareTravel (Maybe Player)
    | PrepareElection (SeqDict Player Euros)
    | PrepareInvestment (Maybe Player) Euros


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
    , investment = ( Nothing, Quantity.zero )
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
        [ el [ Font.bold ] (text "Country")
        , Element.map PrepareCountry (countryPicker model.country)
        , [ ( fill, viewTravel model.country model.travel )
          , ( fill, viewInvestment model.country model.investment )
          , ( fillPortion 2, viewElection model.country model.election )
          ]
            |> List.map
                (\( portion, children ) ->
                    column
                        [ Border.width 1
                        , Theme.padding
                        , alignTop
                        , height fill
                        , width portion
                        ]
                        children
                )
            |> wrappedRow []
        ]


viewTravel : Maybe Country -> Maybe Player -> List (Element Msg)
viewTravel country player =
    [ el [ Font.bold ] (text "Travel")
    , Element.map PrepareTravel (playerPicker player)
    , Theme.primaryButton
        [ width fill
        , alignBottom
        ]
        { label = "Travel"
        , onPress = Maybe.map2 (\p c -> CommitAction (TravelTo p c)) player country
        }
    ]


viewInvestment : Maybe Country -> ( Maybe Player, Euros ) -> List (Element Msg)
viewInvestment country ( player, euros ) =
    [ el [ Font.bold ] (text "Invest")
    , Element.map
        (\newPlayer -> PrepareInvestment newPlayer euros)
        (playerPicker player)
    , Input.text [ width fill ]
        { label = Input.labelLeft [] (text "€")
        , text = String.fromInt (Money.inEuros euros)
        , onChange =
            \newEuros ->
                PrepareInvestment player
                    (newEuros
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


viewElection : Maybe Country -> SeqDict Player Euros -> List (Element Msg)
viewElection country votes =
    let
        playerRow : Player -> Element Msg
        playerRow player =
            let
                previous : Euros
                previous =
                    SeqDict.get player votes
                        |> Maybe.withDefault Quantity.zero
            in
            Input.text [ width fill ]
                { label = Input.labelLeft [] (text "€")
                , text =
                    previous
                        |> Money.inEuros
                        |> String.fromInt
                , onChange =
                    \newEuros ->
                        SeqDict.insert player
                            (newEuros
                                |> String.toInt
                                |> Maybe.map Money.euros
                                |> Maybe.withDefault previous
                            )
                            votes
                            |> PrepareElection
                , placeholder = Nothing
                }

        button : Element Msg
        button =
            Theme.primaryButton
                [ width fill
                , alignBottom
                ]
                { label = "Elect"
                , onPress = Maybe.map (\c -> CommitAction (Election c votes)) country
                }
    in
    el [ Font.bold ] (text "Election")
        :: List.map playerRow Data.players
        ++ [ button ]


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
        |> wrappedRow []


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

module Main exposing (main)

import Browser
import Element as E exposing (Color, Element)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import Element.Input as Input
import Html exposing (Html)
import Html.Attributes


type Card
    = Ingredients
    | Mixing
    | Baking


type alias Model =
    { expandedCard : Maybe Card }


pageTitle : String
pageTitle =
    "How to Make Great Pizza at Home"


main : Program () Model Msg
main =
    Browser.document
        { init = \flags -> ( { expandedCard = Nothing }, Cmd.none )
        , view =
            \model ->
                { title = pageTitle
                , body = [ view model ]
                }
        , update = update
        , subscriptions = \_ -> Sub.none
        }



-- Color palette


black : Color
black =
    E.rgb255 66 66 66


champagne : Color
champagne =
    E.rgb255 251 225 202


yellow : Color
yellow =
    E.rgb255 233 196 106


pumpkin : Color
pumpkin =
    E.rgb255 244 162 97


orange : Color
orange =
    E.rgb255 231 111 81



-- VIEW


view : Model -> Html Msg
view model =
    E.layout
        [ Background.color champagne
        , Font.color black
        , E.behindContent <|
            E.el
                [ E.width E.fill
                , E.height E.fill
                , Events.onClick (ExpandCard Nothing)
                ]
                E.none
        ]
        (E.column
            [ E.width E.fill
            ]
            [ E.row
                [ E.width E.fill
                , E.alignTop
                , Events.onClick (ExpandCard Nothing)
                ]
                [ E.el
                    [ E.paddingXY 30 60
                    , E.centerX
                    , Font.size 37
                    ]
                    (E.text pageTitle)
                ]
            , case model.expandedCard of
                Just card ->
                    viewCard card

                Nothing ->
                    viewHome
            ]
        )


viewHome : Element Msg
viewHome =
    E.row
        [ E.width E.fill
        , E.alignTop
        , E.spacing 30
        , E.paddingXY 30 0
        ]
        [ viewButton Ingredients
        , viewButton Mixing
        , viewButton Baking
        ]


viewCard : Card -> Element Msg
viewCard card =
    E.row [] [ E.el [] (E.text <| cardToString card) ]


cardToString : Card -> String
cardToString card =
    case card of
        Ingredients ->
            "Ingredients"

        Mixing ->
            "Mixing & Kneading"

        Baking ->
            "Baking"


cardColor : Card -> Color
cardColor card =
    case card of
        Ingredients ->
            yellow

        Mixing ->
            pumpkin

        Baking ->
            orange


viewButton : Card -> Element Msg
viewButton card =
    Input.button
        [ Background.color (cardColor card)
        , E.padding 30
        , E.centerX
        ]
        { onPress = Just (ExpandCard (Just card))
        , label = E.text (cardToString card)
        }


type Msg
    = NoOp
    | ExpandCard (Maybe Card)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        ExpandCard card ->
            ( { model | expandedCard = card }, Cmd.none )

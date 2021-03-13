module Main exposing (main)

import Browser
import Element as E exposing (Color, Element)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Html exposing (Html)
import Html.Attributes


type alias Model =
    {}


pageTitle : String
pageTitle =
    "How to Make Great Pizza at Home"


main : Program () Model Msg
main =
    Browser.document
        { init = \flags -> ( {}, Cmd.none )
        , view =
            \model ->
                { title = pageTitle
                , body = [ view model ]
                }
        , update = update
        , subscriptions = \_ -> Sub.none
        }



-- Color palette


white : Color
white =
    E.rgb255 255 255 255


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
    E.layout [ Background.color champagne, Font.color black ] <|
        E.column
            [ E.width E.fill
            , E.padding 30
            ]
        <|
            [ E.row
                [ E.width E.fill
                , E.alignTop
                ]
                [ E.el
                    [ E.padding 30
                    , E.centerX
                    , Font.size 37
                    ]
                    (E.text pageTitle)
                ]
            , E.row
                [ E.width E.fill
                , E.alignTop
                , E.spacing 30
                , E.padding 30
                ]
              <|
                [ card [ Background.color yellow ] "Ingredients"
                , card [ Background.color pumpkin ] "Mixing & Kneading"
                , card [ Background.color orange ] "Baking"
                ]
            ]


card : List (E.Attribute Msg) -> String -> Element Msg
card attrs title =
    Input.button
        ([ Background.color (E.rgb255 230 20 240)
         , E.padding 30
         , E.pointer
         , E.centerX
         ]
            ++ attrs
        )
        { onPress = Nothing
        , label = E.text title
        }


type Msg
    = NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

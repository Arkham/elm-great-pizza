module Main exposing (main)

import Browser
import Element as E exposing (Element)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import Element.Input as Input
import Html exposing (Html)
import Html.Attributes
import Set exposing (Set)


type Card
    = Ingredients
    | Mixing
    | Baking


type alias Model =
    { expandedCard : Maybe Card
    , doughIngredients : Set String
    , sauceIngredients : Set String
    , mixingSteps : Set String
    , bakingSteps : Set String
    }


pageTitle : String
pageTitle =
    "🍕 How to Make Great Pizza at Home 🍕"


initialModel : Model
initialModel =
    { expandedCard = Nothing
    , doughIngredients = Set.empty
    , sauceIngredients = Set.empty
    , mixingSteps = Set.empty
    , bakingSteps = Set.empty
    }


main : Program () Model Msg
main =
    Browser.document
        { init = \flags -> ( initialModel, Cmd.none )
        , view =
            \model ->
                { title = pageTitle
                , body = [ view model ]
                }
        , update = update
        , subscriptions = \_ -> Sub.none
        }



-- Color palette


black : E.Color
black =
    E.rgb255 66 66 66


champagne : E.Color
champagne =
    E.rgb255 251 225 202


yellow : E.Color
yellow =
    E.rgb255 233 196 106


pumpkin : E.Color
pumpkin =
    E.rgb255 244 162 97


orange : E.Color
orange =
    E.rgb255 231 111 81



-- VIEW


view : Model -> Html Msg
view model =
    E.layout
        [ Background.color champagne
        , Font.color black
        ]
        (E.column
            [ E.width E.fill
            , E.height E.fill
            , E.paddingXY 0 60
            , E.spacing 40
            , E.behindContent <|
                E.el
                    [ E.width E.fill
                    , E.height E.fill
                    , Events.onClick (ExpandCard Nothing)
                    ]
                    E.none
            ]
            [ E.row
                [ E.width E.fill
                , E.alignTop
                , Events.onClick (ExpandCard Nothing)
                ]
                [ E.el
                    [ E.padding 30
                    , E.centerX
                    , Font.size 37
                    ]
                    (E.text pageTitle)
                ]
            , E.row [ E.centerX ] [ viewCard Ingredients model ]
            , E.row [ E.centerX ] [ viewCard Mixing model ]
            , E.row [ E.centerX ] [ viewCard Baking model ]
            , E.row
                [ E.alignBottom
                , E.centerX
                , Font.size 16
                ]
                [ E.text "Made with ❤️ and 🍕 by "
                , E.link [ Font.underline ]
                    { url = "https://twitter.com/arkh4m", label = E.text "arkh4m" }
                ]
            ]
        )


cardToString : Card -> String
cardToString card =
    case card of
        Ingredients ->
            "Ingredients"

        Mixing ->
            "Mixing & Kneading"

        Baking ->
            "Baking"


cardColor : Card -> E.Color
cardColor card =
    case card of
        Ingredients ->
            yellow

        Mixing ->
            pumpkin

        Baking ->
            orange


viewCard : Card -> Model -> Element Msg
viewCard card model =
    let
        attrs =
            [ Background.color (cardColor card)
            , E.padding 30
            , E.centerX
            , E.alignTop
            ]
    in
    if model.expandedCard == Just card then
        E.el (E.width (E.px 640) :: attrs)
            (E.column [ E.spacing 20 ]
                [ E.row [ Font.bold ] [ E.text (cardToString card) ]
                , case card of
                    Ingredients ->
                        viewIngredients model

                    Mixing ->
                        viewMixing model

                    Baking ->
                        viewBaking model
                ]
            )

    else
        Input.button (E.width (E.px 250) :: Font.center :: attrs)
            { onPress = Just (ExpandCard (Just card))
            , label = E.text (cardToString card)
            }


type Unit
    = Gram Float
    | Ml Float
    | Tbsp Float
    | Tsp Float
    | ToTaste


unitToString : Unit -> String
unitToString unit =
    case unit of
        Gram v ->
            String.fromFloat v ++ " g"

        Ml v ->
            String.fromFloat v ++ " ml"

        Tbsp v ->
            String.fromFloat v ++ " tbsp"

        Tsp v ->
            String.fromFloat v ++ " tsp"

        ToTaste ->
            "to taste"


type alias Ingredient =
    ( Unit, String )


doughIngredients : List Ingredient
doughIngredients =
    [ ( Gram 200, "Strong white flour" )
    , ( Gram 300, "Plain white flour" )
    , ( Ml 300, "Water" )
    , ( Gram 3.5, "Dry yeast" )
    , ( Gram 35, "Extra virgin olive oil" )
    , ( Gram 10, "Fine salt" )
    ]


sauceIngredients : List Ingredient
sauceIngredients =
    [ ( Gram 500, "Tomato passata (Cirio / Mutti)" )
    , ( Tbsp 1, "Oregano" )
    , ( Gram 400, "Mozzarella" )
    , ( Tsp 1, "Fine salt" )
    , ( ToTaste, "Extra virgin olive oil" )
    , ( ToTaste, "Basil" )
    ]


viewIngredients : Model -> Element Msg
viewIngredients model =
    E.column [ E.spacing 15 ]
        [ E.el [ Font.italic ] (E.text "Dough for 2 pizzas")
        , E.column [ E.spacing 10 ]
            (List.map
                (\(( unit, name ) as ingredient) ->
                    Input.checkbox []
                        { onChange = ToggleDoughIngredient name
                        , icon = Input.defaultCheckbox
                        , checked = Set.member name model.doughIngredients
                        , label = Input.labelRight [] (ingredientLabel ingredient)
                        }
                )
                doughIngredients
            )
        , E.el [ Font.italic ] (E.text "Sauce")
        , E.column [ E.spacing 10 ]
            (List.map
                (\(( unit, name ) as ingredient) ->
                    Input.checkbox []
                        { onChange = ToggleSauceIngredient name
                        , icon = Input.defaultCheckbox
                        , checked = Set.member name model.sauceIngredients
                        , label = Input.labelRight [] (ingredientLabel ingredient)
                        }
                )
                sauceIngredients
            )
        ]


ingredientLabel : ( Unit, String ) -> Element msg
ingredientLabel ( unit, name ) =
    E.row
        [ Font.size 18, E.spacing 5 ]
        [ E.el [ Font.semiBold ] (E.text name)
        , E.text (unitToString unit)
        ]


mixingInstructions : List String
mixingInstructions =
    [ "Pour lukewarm water into a cup, add yeast and let dissolve."
    , "Combine the two flours in a bowl, then mix in the water and yeast."
    , "(Optional) Let the mix rest for half an hour."
    , "Add salt and knead until the dough stops sticking to your hands."
    , "Add extra virgin olive oil and keep kneading until all oil is absorbed."
    , "The surface of your dough should be smooth and elastic. If it's not, keep kneading!"
    , "Now do a fold. Stretch the dough in one direction and close it on itself. Then, rotate by 90 degrees and repeat the fold. Repeat this process 3 more times."
    , "You're all done now! Let the dough rest for 4-6 hours at room temperature or for 24 hours in the fridge."
    ]


bakingInstructions : List String
bakingInstructions =
    [ "If you did put the dough in the fridge, take it out 2 hours beforehand."
    , "Cut the dough into smaller balls, fold them until they become elastic."
    , "Cover the balls with plastic wrap and let rest for 30 minutes."
    , "In the meanwhile preheat your oven to 250° C in static mode (no fan)."
    , "Prepare the sauce by mixing tomato passata, salt, olive oil, and oregano."
    , "Break down the mozzarella into small bits and let them dry in a strainer."
    , "When the oven is hot, dust your workspace with some flour and start stretching the dough."
    , "Now put some tomato sauce on the pizza and bake for 6-7 minutes."
    , "Take the pizza out of the oven, add mozzarella and toppings, and bake for another 6-7 minutes. Time to eat!"
    ]


viewMixing : Model -> Element Msg
viewMixing model =
    E.column [ E.spacing 20 ] <|
        List.map
            (\step ->
                Input.checkbox []
                    { onChange = ToggleMixingStep step
                    , icon = Input.defaultCheckbox
                    , checked = Set.member step model.mixingSteps
                    , label =
                        Input.labelRight [ E.paddingXY 5 0, E.width E.fill ]
                            (E.paragraph [ Font.size 16 ] [ E.text step ])
                    }
            )
            mixingInstructions


viewBaking : Model -> Element Msg
viewBaking model =
    E.column [ E.spacing 20 ] <|
        List.map
            (\step ->
                Input.checkbox []
                    { onChange = ToggleBakingStep step
                    , icon = Input.defaultCheckbox
                    , checked = Set.member step model.bakingSteps
                    , label =
                        Input.labelRight [ E.paddingXY 5 0, E.width E.fill ]
                            (E.paragraph [ Font.size 16 ] [ E.text step ])
                    }
            )
            bakingInstructions



-- UPDATE


type Msg
    = NoOp
    | ExpandCard (Maybe Card)
    | ToggleDoughIngredient String Bool
    | ToggleSauceIngredient String Bool
    | ToggleMixingStep String Bool
    | ToggleBakingStep String Bool


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        ExpandCard card ->
            ( { model | expandedCard = card }
            , Cmd.none
            )

        ToggleDoughIngredient name value ->
            ( { model
                | doughIngredients =
                    toggleSet name value model.doughIngredients
              }
            , Cmd.none
            )

        ToggleSauceIngredient name value ->
            ( { model
                | sauceIngredients =
                    toggleSet name value model.sauceIngredients
              }
            , Cmd.none
            )

        ToggleMixingStep name value ->
            ( { model
                | mixingSteps =
                    toggleSet name value model.mixingSteps
              }
            , Cmd.none
            )

        ToggleBakingStep name value ->
            ( { model
                | bakingSteps =
                    toggleSet name value model.bakingSteps
              }
            , Cmd.none
            )


toggleSet : String -> Bool -> Set String -> Set String
toggleSet name value set =
    if value then
        Set.insert name set

    else
        Set.remove name set

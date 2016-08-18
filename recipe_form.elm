module Main exposing (..)

import Html exposing (..)
import Html.App as App
import Html.Attributes exposing (..)
import Html.Events exposing (onInput, onClick)


main =
    App.beginnerProgram
        { model = init
        , update = update
        , view = view
        }



-- MODEL


type alias Model =
    { title : String
    , ingredient : String
    , instruction : String
    , ingredients : List String
    , instructions : List String
    }


init : Model
init =
    { title = ""
    , ingredient = ""
    , instruction = ""
    , ingredients = []
    , instructions = []
    }



-- UPDATE


type Msg
    = Title String
    | Ingredient String
    | Instruction String
    | AddIngredient String
    | AddInstruction String
    | RemoveIngredient String
    | RemoveInstruction String


update : Msg -> Model -> Model
update msg model =
    case msg of
        Title title ->
            { model | title = title }

        Ingredient ingredient ->
            { model | ingredient = ingredient }

        Instruction instruction ->
            { model | instruction = instruction }

        AddIngredient ingredient ->
            { model | ingredients = model.ingredients ++ [ ingredient ], ingredient = "" }

        AddInstruction instruction ->
            { model | instructions = model.instructions ++ [ instruction ] }

        RemoveIngredient ingredient ->
            { model | ingredients = List.filter (\n -> (n /= ingredient)) model.ingredients }

        RemoveInstruction instruction ->
            { model | instructions = List.filter (\n -> (n /= instruction)) model.instructions }



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ p []
            [ input [ type' "text", placeholder "Title", onInput Title ] []
            ]
        , p []
            [ input [ type' "text", placeholder "10 Granny Smith Apples...", value model.ingredient, onInput Ingredient ] []
            , button [ onClick (AddIngredient model.ingredient) ] [ text "Add Ingredient" ]
            ]
        , p []
            [ input [ type' "text", placeholder "Peel the apples...", onInput Instruction ] []
            , button [ onClick (AddInstruction model.instruction) ] [ text "Add Instruction" ]
            ]
        , h1 [] [ text model.title ]
        , div [] <|
            recipeFilter
                [ (,) (List.length model.ingredients > 0) <| h3 [] [ text "Ingredients" ]
                , (,) True <| listIngredients model
                , (,) (List.length model.instructions > 0) <| h3 [] [ text "Instructions" ]
                , (,) True <| listInstructions model
                ]
        ]


recipeFilter : List ( Bool, a ) -> List a
recipeFilter list =
    List.filterMap
        (\( bool, a ) ->
            if bool == True then
                Just a
            else
                Nothing
        )
        list


listIngredients : Model -> Html Msg
listIngredients model =
    let
        parseIngredient : String -> Html Msg
        parseIngredient ingredient =
            li []
                [ text ingredient
                , button [ onClick (RemoveIngredient ingredient) ] [ text "Remove" ]
                ]
    in
        ul [] (List.map parseIngredient model.ingredients)


listInstructions : Model -> Html Msg
listInstructions model =
    let
        parseInstruction : String -> Html Msg
        parseInstruction instruction =
            li []
                [ text instruction
                , button [ onClick (RemoveInstruction instruction) ] [ text "Remove" ]
                ]
    in
        ul [] (List.map parseInstruction model.instructions)

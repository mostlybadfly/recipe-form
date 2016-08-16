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
    , instructions: List String
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
            { model | ingredients = model.ingredients ++ [ ingredient ] }

        AddInstruction instruction ->
            { model | instructions = model.instructions ++ [ instruction ] }



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ div []
            [ input [ type' "text", placeholder "Title", onInput Title ] []
            ]
        , div []
            [ input [ type' "text", placeholder "Ingredients", onInput Ingredient ] []
            , button [ onClick (AddIngredient model.ingredient) ] [ text "Add Ingredient" ]
            ]
        , div []
            [ input [ type' "text", placeholder "Instructions", onInput Instruction ] []
            , button [ onClick (AddInstruction model.instruction) ] [ text "Add Instruction" ]
            ]
        , h1 [] [ text model.title ]
        , h3 [] [ text "Ingredients" ]
        , listIngredients model
        , h3 [] [ text "Instructions" ]
        , listInstructions model
        ]


listIngredients : Model -> Html msg
listIngredients model =
    let
        parseIngredient : String -> Html msg
        parseIngredient ingredient =
            li [] [ text ingredient ]
    in
        ul [] (List.map parseIngredient model.ingredients)

listInstructions : Model -> Html msg
listInstructions model =
  let
      parseInstruction : String -> Html msg
      parseInstruction instruction =
        li [] [ text instruction ]
  in
      ul [] (List.map parseInstruction model.instructions)

module Main exposing (..)

import Html exposing (..)
import Html.App as App
import Html.Attributes exposing (..)
import Html.Events exposing (onInput, onClick, onBlur, on, keyCode)
import Json.Decode as Json


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
    , recipes : List Recipe
    , currentRecipe : Maybe Recipe
    , uid : Int
    }


init : Model
init =
    { title = ""
    , ingredient = ""
    , instruction = ""
    , ingredients = []
    , instructions = []
    , recipes = []
    , uid = 0
    , currentRecipe = Nothing
    }


type alias Recipe =
    { id : Int
    , title : String
    , ingredients : List String
    , instructions : List String
    }


newRecipe : Int -> String -> List String -> List String -> Recipe
newRecipe id title ingredients instructions =
    { id = id
    , title = title
    , ingredients = ingredients
    , instructions = instructions
    }


currentRecipe : Int -> String -> List String -> List String -> Recipe
currentRecipe id title ingredients instructions =
    { id = id
    , title = title
    , ingredients = ingredients
    , instructions = instructions
    }



-- UPDATE


type Msg
    = NoOp
    | Title String
    | Ingredient String
    | Instruction String
    | AddIngredient String
    | AddInstruction String
    | RemoveIngredient String
    | RemoveInstruction String
    | AddRecipe
    | GetRecipe Int


update : Msg -> Model -> Model
update msg model =
    case msg of
        NoOp ->
            model

        Title title ->
            { model | title = title }

        Ingredient ingredient ->
            { model | ingredient = ingredient }

        Instruction instruction ->
            { model | instruction = instruction }

        AddIngredient ingredient ->
            { model
                | ingredients =
                    model.ingredients ++ [ ingredient ]
                , ingredient = ""
            }

        AddInstruction instruction ->
            { model
                | instructions =
                    model.instructions ++ [ instruction ]
                , instruction = ""
            }

        RemoveIngredient ingredient ->
            { model
                | ingredients =
                    List.filter (\n -> (n /= ingredient)) model.ingredients
            }

        RemoveInstruction instruction ->
            { model
                | instructions =
                    List.filter (\n -> (n /= instruction)) model.instructions
            }

        AddRecipe ->
            { model
                | recipes = model.recipes ++ [ newRecipe model.uid model.title model.ingredients model.instructions ]
                , title = ""
                , ingredients = []
                , instructions = []
                , uid = model.uid + 1
            }

        GetRecipe id ->
            { model | currentRecipe = List.head (List.filter (\n -> (n.id == id)) model.recipes) }


onEnter : Msg -> Attribute Msg
onEnter msg =
    let
        tagger code =
            if code == 13 then
                msg
            else
                NoOp
    in
        on "keydown" (Json.map tagger keyCode)



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ input
            [ type' "text"
            , class "title"
            , placeholder "Title"
            , value model.title
            , onInput Title
            ]
            []
        , div [] <|
            recipeFilter
                [ (,) True <| h2 [] [ text "Ingredients" ]
                , (,) True <|
                    input
                        [ type' "text"
                        , class "item-input"
                        , placeholder "10 Granny Smith Apples..."
                        , value model.ingredient
                        , onInput Ingredient
                        , onEnter (AddIngredient model.ingredient)
                        ]
                        []
                , (,) True <| listIngredients model
                , (,) True <| h2 [] [ text "Instructions" ]
                , (,) True <|
                    input
                        [ type' "text"
                        , class "item-input"
                        , placeholder "Peel the apples..."
                        , value model.instruction
                        , onInput Instruction
                        , onEnter (AddInstruction model.instruction)
                        ]
                        []
                , (,) True <| listInstructions model
                ]
        , button [ class "add-button", onClick (AddRecipe) ] [ text "Add Recipe" ]
        , listRecipes model
        , showRecipe model.currentRecipe
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
                , button
                    [ class "remove-btn"
                    , onClick (RemoveIngredient ingredient)
                    ]
                    [ text "Remove" ]
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
                , button
                    [ class "remove-btn"
                    , onClick (RemoveInstruction instruction)
                    ]
                    [ text "Remove" ]
                ]
    in
        ul [] (List.map parseInstruction model.instructions)


listRecipes : Model -> Html Msg
listRecipes model =
    let
        parseRecipe : Recipe -> Html Msg
        parseRecipe recipe =
            p [ onClick (GetRecipe recipe.id) ] [ text recipe.title ]
    in
        div []
            [ h3 [] [ text "Saved Recipes:" ]
            , div [] (List.map parseRecipe model.recipes)
            ]


showRecipe : Maybe Recipe -> Html Msg
showRecipe maybeRecipe =
    case maybeRecipe of
        Just recipe ->
            div [ class "recipe-display" ]
                [ p [ class "title" ] [ text recipe.title ]
                , h2 [] [ text "Ingredients" ]
                , p [] [ listRecipeIngredients recipe ]
                , h2 [] [ text "Instructions" ]
                , p [] [ listRecipeInstructions recipe ]
                ]

        Nothing ->
            p [ class "recipe-display" ] [ text "select a recipe" ]


listRecipeIngredients : Recipe -> Html Msg
listRecipeIngredients recipe =
    let
        parseRecipeIngredient : String -> Html Msg
        parseRecipeIngredient ingredient =
            li []
                [ text ingredient
                ]
    in
        ul [] (List.map parseRecipeIngredient recipe.ingredients)


listRecipeInstructions : Recipe -> Html Msg
listRecipeInstructions recipe =
    let
        parseRecipeInstruction : String -> Html Msg
        parseRecipeInstruction instruction =
            li []
                [ text instruction
                ]
    in
        ul [] (List.map parseRecipeInstruction recipe.instructions)

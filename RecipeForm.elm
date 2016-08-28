module Main exposing (..)

import Html exposing (..)
import Html.App as App
import Html.Attributes exposing (..)
import Html.Events exposing (onInput, onClick, onBlur, on, keyCode)
import Json.Decode as Decode exposing ((:=), object4, list,  string, decodeString) 
import Json.Decode as Json
import String exposing (isEmpty)


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
    , recipes = Result.withDefault [] decodedRecipes
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

recipeData = """[{
    "id": 0,
    "title": "Mole",
    "ingredients": [
      "10 chiles guajillo",
      "10 chiles ancho",
      "10 chiles pasilla",
      "5 chiles cascabel",
      "4 tomatillos, peeled",
      "4 cloves garlic",
      "1 ripe plantain",
      "1/2 cup reserved chile seeds",
      "1/2 cup almonds",
      "1/2 cup pumpkin seeds",
      "1/2 cup sesame seeds",
      "1/2 cup raisins",
      "1/2 a stick of canela",
      "3 allspice berries",
      "4 cloves",
      "6 peppercorns",
      "1 tsp cumin seeds",
      "2 tsp marjoram",
      "2 tsp Mexican oregano",
      "1 tsp thyme",
      "2 tortillas",
      "1 bolillo",
      "1/2 cup lard",
      "1 tablet Mexican chocolate",
      "1-2 tsp sugar",
      "5-6 cups chicken broth",
      "salt"
    ],
    "instructions": [
      "Toast all ingredients on a comal until golden brown, except for chocolate.  For small seeds, spices, herbs a sauté pan might work best and heat through until fragrant.",
      "For chiles specifically: toast ancho and pasilla for 2-3 minutes on each side. Toast guajillo and cascabel for 4-5 minutes to get them very dark/black. Place in a bowl and cover with boiled water for 25 minutes.",
      "The plantain can be griddled skin on, the heat will soften it.",
      "For the bolillo: heat the lard until hot, fry the bread well on all sides.",
      "In a blender, start with chiles and nuts/seeds as these will get the thickest.  Add just enough water at a time to get the blender going, you may need to push ingredients down.",
      "Blend the remaining ingredients, except for the chocolate, chicken, and lard. At this point, the sauce should be a slightly thinner paste",
      "Heat the lard in a large pot or dutch oven until very hot.",
      "In one motion, pour all of the mole over the lard, the idea is to fry the sauce and cook it really well. Stir occasionally for 5 minutes.",
      "After this point, add the chocolate and stir well so it can break up and get incorporated.",
      "Add chicken broth and allow this to simmer for an hour, stir frequently to avoid sticking.",
      "Towards the end, use a stick blender to smooth out the sauce a bit more.",
      "This can be poured over chicken or turkey. I prefer to shred chicken and mix in with the sauce."
    ]
  },
  {
    "id": 1,
    "title": "Coconut Milk Braised Greens",
    "ingredients": [
      "5 bunches collard greens",
      "2 bunches turnip greens",
      "3 leeks, sliced in half",
      "1 large knob ginger, crushed",
      "1 head garlic",
      "4 shallots",
      "32 oz vegetable broth",
      "1 large yellow onion",
      "4 cloves garlic, minced",
      "4 cloves garlic, minced",
      "2-3 tablespoons gochujang",
      "2 cans coconut milk",
      "1-2 tablespoons rice vinegar",
      "1-2 tablespoons white vinegar",
      "salt and pepper"
    ],
    "instructions": [
      "Begin by roasting sliced leeks, crushed garlic, unpeeled garlic cloves and shallots in a very hot oven. The goal is to get a nice dark brown color or even some charring on them. Turn halfway through to get color on the other side.",
      "Add all of the roasted aromatics to a pot with the vegetable broth and bring to a boil, simmer for at least an hour.",
      "In the meantime, remove stems from all of the greens and wash by hand. This is easiest in a clean sink or a large container of water. Once this is done, lay the greens flat, starting with the biggest leaves, maybe 10-15 leaves each. Roll each pile of leaves into cigars and cut across to create ribbons.  Place these in a large container filled with water to rinse once last time.",
      "Strain the broth, in the same pan sauté onion, minced garlic and ginger until softened.  Add the gochujang to and cook for another minute before adding the vegetable broth and coconut milk, bring to a boil for 15 minutes.",
      "Add all of the greens, if there is not room stir to start wilting them, they should reduce in size significantly.  Reduce heat to low and simmer for two hours, stirring occasionally.",
      "To finish, stir in vinegars as well as additional salt and pepper to taste."
    ]
  }]"""

cookbookDecoder : Decode.Decoder (List Recipe)
cookbookDecoder = Decode.list recipeDecoder

recipeDecoder : Decode.Decoder Recipe
recipeDecoder =
  Decode.object4 Recipe
  ("id" := Decode.int)
  ("title" := Decode.string)
  ("ingredients" := Decode.list string)
  ("instructions" := Decode.list string)

decodedRecipes : Result String (List Recipe)
decodedRecipes = decodeString cookbookDecoder recipeData


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
            if isEmpty ingredient == True then
                model
            else
                { model
                    | ingredients = model.ingredients ++ [ ingredient ]
                    , ingredient = ""
                }

        AddInstruction instruction ->
            if isEmpty instruction == True then
                model
            else
                { model
                    | instructions = model.instructions ++ [ instruction ]
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
        , div []
            [ h2 [] [ text "Ingredients" ]
            , input
                [ type' "text"
                , class "item-input"
                , placeholder "10 Granny Smith Apples..."
                , value model.ingredient
                , onInput Ingredient
                , onEnter (AddIngredient model.ingredient)
                ]
                []
            , listIngredients model
            , h2 [] [ text "Instructions" ]
            , input
                [ type' "text"
                , class "item-input"
                , placeholder "Peel the apples..."
                , value model.instruction
                , onInput Instruction
                , onEnter (AddInstruction model.instruction)
                ]
                []
            , listInstructions model
            , p [] <|
                recipeFilter
                    [ (,) (model.title /= "") <| button [ class "add-button", onClick (AddRecipe) ] [ text "Add Recipe" ]
                    ]
            ]
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
                    [ class "remove-button"
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
                    [ class "remove-button"
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
                [ h1 [] [ text recipe.title ]
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
            li [ class "list-item" ]
                [ text ingredient
                ]
    in
        ul [] (List.map parseRecipeIngredient recipe.ingredients)


listRecipeInstructions : Recipe -> Html Msg
listRecipeInstructions recipe =
    let
        parseRecipeInstruction : String -> Html Msg
        parseRecipeInstruction instruction =
            li [ class "list-item" ]
                [ text instruction
                ]
    in
        ol [] (List.map parseRecipeInstruction recipe.instructions)

module Main exposing (Model, Msg(..), add, main, sumUp, update, view, viewNumber)

import Browser
import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)


main : Program () Model Msg
main =
    Browser.sandbox
        { init = Model Nothing "" Dict.empty
        , view = view
        , update = update
        }


type alias Model =
    { nextNumber : Maybe Int
    , nextCategory : String
    , numbers : Dict String Int
    }


type Msg
    = Add
    | UpdatedNumber String
    | UpdatedCategory String


update : Msg -> Model -> Model
update msg model =
    case msg of
        UpdatedNumber num ->
            { model | nextNumber = String.toInt num }

        UpdatedCategory category ->
            { model | nextCategory = category }

        Add ->
            let
                nextNumber =
                    case model.nextNumber of
                        Just num ->
                            num

                        Nothing ->
                            0

                newNumbers =
                    Dict.update model.nextCategory (add nextNumber) model.numbers
            in
            { model | numbers = newNumbers }


view : Model -> Html Msg
view model =
    div []
        [ h1 [] [ text "Christmas Calculator" ]
        , input [ placeholder "new number", onInput UpdatedNumber ] []
        , input [ placeholder "category", onInput UpdatedCategory ] []
        , button [ onClick Add ] [ text "add" ]
        , div []
            [ ul [] (List.map viewNumber (Dict.toList model.numbers))
            ]
        , div []
            [ h2 [] [ text ("Total: " ++ String.fromInt (sumUp model.numbers)) ]
            ]
        ]


viewNumber : ( String, Int ) -> Html Msg
viewNumber pair =
    li []
        [ text (Tuple.first pair ++ " - " ++ String.fromInt (Tuple.second pair))
        ]


add : Int -> Maybe Int -> Maybe Int
add newValue maybeOldValue =
    case maybeOldValue of
        Just oldValue ->
            Just (oldValue + newValue)

        Nothing ->
            Just newValue


sumUp : Dict String Int -> Int
sumUp numbers =
    List.foldl (+) 0 (Dict.values numbers)

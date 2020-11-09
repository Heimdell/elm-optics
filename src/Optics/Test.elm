module Optics.Test exposing (..)

import Dict exposing (Dict)
import Html exposing (Html)
import Optics.Basic exposing (..)
import Optics.Core exposing (..)


type alias Point =
    { x : Float, y : Float }


type alias Block =
    { mass : Float, components : List ( Maybe Point, String ) }


type alias Model =
    Dict String Block


y_ : SimpleLens ls { a | y : b } b
y_ =
    lens .y <| \s a -> { s | y = a }


components_ : SimpleLens ls { a | components : b } b
components_ =
    lens .components <| \s a -> { s | components = a }


top : Model -> Maybe Float
top =
    getAll (o dictValues (o components_ (o each (o first (o just_ y_)))))
        >> List.maximum


moveUp : Float -> Model -> Model
moveUp y =
    over (o (o dictValues components_) (o each (o first (o just_ y_)))) (\it -> it + y)


test1 : Int
test1 =
    ( ( 1, 2 ), 3 )
        |> get (o first second)


test2 : Maybe Int
test2 =
    [ ( Just ( 1, 2 ), 3 ) ]
        |> getSome (o each (o first (o just_ second)))


test3 : List ( Maybe ( Int, String ), Int )
test3 =
    [ ( Just ( 1, 2 ), 3 ) ]
        |> over (o (o each first) (o just_ second)) Debug.toString


test4 : Maybe (Maybe Int)
test4 =
    1 |> review (o just_ just_)


eq : String -> a -> a -> Html b
eq msg a b =
    Html.div []
        [ Html.text <|
            if a == b then
                msg ++ ": Pass"

            else
                msg ++ ": " ++ Debug.toString a ++ " != " ++ Debug.toString b
        ]


main : Html a
main =
    Html.ul []
        [ eq "deep access" test1 2
        , eq "composition" test2 (Just 2)
        , eq "deep over" test3 [ ( Just ( 1, "2" ), 3 ) ]
        , eq "reconstruction" test4 (Just (Just 1))
        ]

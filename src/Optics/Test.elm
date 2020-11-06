module Optics.Test exposing (..)

import Html exposing (Html)
import Html.Attributes exposing (accesskey)
import Optics.Basic exposing (..)
import Optics.Core exposing (..)


test1 : Int
test1 =
    ( ( 1, 2 ), 3 )
        |> view (o first second)


test2 : Maybe Int
test2 =
    [ ( Just ( 1, 2 ), 3 ) ]
        |> viewSome (o each (o first (o just_ second)))


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
        , eq "deep update" test3 [ ( Just ( 1, "2" ), 3 ) ]
        , eq "reconstruction" test4 (Just (Just 1))
        ]

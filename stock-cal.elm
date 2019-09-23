module Main exposing (Model, Msg(..), init, main, toCents, update, view, viewInput)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)



-- MAIN


main =
    Browser.sandbox { init = init, update = update, view = view }



-- MODEL


type alias Model =
    { avaliableCash : String
    , pricePerStock : String
    }



--INIT


init : Model
init =
    Model "" ""



-- UPDATE


type Msg
    = AvaliableCash String
    | PricePerStock String


update : Msg -> Model -> Model
update msg model =
    case msg of
        AvaliableCash avaliableCash ->
            { model | avaliableCash = avaliableCash }

        PricePerStock pricePerStock ->
            { model | pricePerStock = pricePerStock }



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ viewInput "text" "total cash" model.avaliableCash AvaliableCash
        , viewInput "text" "price per stock" model.pricePerStock PricePerStock
        , div [] [ text "Max # of stocks:" ]
        , div [] [ text <| String.fromInt (toCents model.avaliableCash // toCents model.pricePerStock) ]
        ]


viewInput : String -> String -> String -> (String -> msg) -> Html msg
viewInput t p v toMsg =
    input [ type_ t, placeholder p, value v, onInput toMsg ] []


toCents : String -> Int
toCents val =
    Maybe.withDefault 0.0 (String.toFloat val) |> (*) 100 |> Basics.truncate


commissionPercent : Float
commissionPercent =
    0.02


tradeFee : Int
tradeFee =
    100000


cessFeePercent : Float
cessFeePercent =
    0.3

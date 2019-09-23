module Main exposing (Model, Msg(..), init, main, update, view)

import Browser
import Decimal exposing (Decimal)
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
        , div [] [ text <| calculateMaxShares model.avaliableCash model.pricePerStock ]
        ]


viewInput : String -> String -> String -> (String -> msg) -> Html msg
viewInput t p v toMsg =
    input [ type_ t, placeholder p, value v, onInput toMsg ] []


toCents : String -> Decimal
toCents val =
    Maybe.withDefault (Decimal.fromInt 0) (Decimal.fromString val)


commissionPercent : Decimal
commissionPercent =
    Maybe.withDefault (Decimal.fromInt -1) (0.02 |> Decimal.fromFloat)


tradeFee : Decimal
tradeFee =
    1000 |> Decimal.fromInt


cessFeePercent : Decimal
cessFeePercent =
    Maybe.withDefault (Decimal.fromInt -1) (0.3 |> Decimal.fromFloat)


gctPercent : Decimal
gctPercent =
    Maybe.withDefault (Decimal.fromInt -1) (16.5 |> Decimal.fromFloat)


calculateMaxShares : String -> String -> String
calculateMaxShares avaliableCash pricePerStock =
    let
        cash =
            toCents avaliableCash

        price =
            toCents pricePerStock

        maxAmount =
            Maybe.withDefault (Decimal.fromInt 0) (Decimal.fastdiv cash price)

        minAmountPrice =
            price |> Decimal.mul (toCents "100")
    in
    if Decimal.gt minAmountPrice cash then
        "Not enought funds to buy shares."

    else
        Decimal.truncate 0 maxAmount |> Decimal.toString

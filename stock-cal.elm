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

        -- , div [ style "font-family" "sans-serif" ] [ text <| String.append "Total for 10,000 shares at $1.75: $" (Decimal.toString <| fullCal 10000 (Maybe.withDefault (Decimal.minusOne) (1.75 |> Decimal.fromFloat))) ]
        ]


viewInput : String -> String -> String -> (String -> msg) -> Html msg
viewInput t p v toMsg =
    input [ type_ t, placeholder p, value v, onInput toMsg ] []


strToDecimal : String -> Decimal
strToDecimal val =
    Maybe.withDefault Decimal.zero (Decimal.fromString val)


commission : Decimal
commission =
    Maybe.withDefault Decimal.minusOne (0.0002 |> Decimal.fromFloat)


tradeFee : Decimal
tradeFee =
    1000 |> Decimal.fromInt


cessFee : Decimal
cessFee =
    Maybe.withDefault Decimal.minusOne (0.0033 |> Decimal.fromFloat)


gct : Decimal
gct =
    Maybe.withDefault Decimal.minusOne (0.165 |> Decimal.fromFloat)


minShares : Decimal
minShares =
    Decimal.fromInt 100


minCommission : Decimal
minCommission =
    Decimal.fromInt 500


calculateMaxShares : String -> String -> String
calculateMaxShares avaliableCash pricePerStock =
    let
        cash =
            Decimal.truncate -2 (strToDecimal avaliableCash)

        price =
            Decimal.truncate -2 (strToDecimal pricePerStock)

        maxShares =
            Maybe.withDefault Decimal.zero (Decimal.fastdiv cash price)

        minAmountPrice =
            fullCal minShares price
    in
    if Decimal.gt minAmountPrice cash then
        "Not enought funds to buy shares."

    else
        Decimal.truncate 0 (findMaxShares maxShares price cash) |> Decimal.toString


findMaxShares : Decimal -> Decimal -> Decimal -> Decimal
findMaxShares maxShares price cash =
    price


fullCal : Decimal -> Decimal -> Decimal
fullCal numOfShares pricePerStock =
    let
        grossPrice =
            Decimal.mul pricePerStock numOfShares

        finalCommission =
            if Decimal.lte (Decimal.mul grossPrice commission) minCommission then
                minCommission

            else
                pricePerStock

        cess =
            Decimal.mul grossPrice cessFee

        trade =
            Decimal.zero

        gctTax =
            Decimal.add finalCommission cess |> Decimal.add trade |> Decimal.mul gct
    in
    Decimal.add grossPrice cess
        |> Decimal.add trade
        |> Decimal.add gctTax
        |> Decimal.add finalCommission
        |> Decimal.round -2

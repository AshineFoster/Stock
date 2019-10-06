module Main exposing (Model, Msg(..), init, main, update, view)

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

        -- , div [] [ text "Max # of stocks:" ]
        , div [] [ text <| calculateMaxShares model.avaliableCash model.pricePerStock ]

        -- , div [] [ text <| Decimal.toString <| fullCal (Decimal.fromInt 10000) (Maybe.withDefault Decimal.minusOne (1.75 |> Decimal.fromFloat)) ]
        ]


viewInput : String -> String -> String -> (String -> msg) -> Html msg
viewInput t p v toMsg =
    input [ type_ t, placeholder p, value v, onInput toMsg ] []


toCents : String -> Int
toCents val =
    Maybe.withDefault 0 (String.toFloat val)
        |> (*) 100
        |> truncate


dolToCents : Float -> Int
dolToCents dollars =
    dollars * 100.0 |> truncate


toDollars : Int -> Float
toDollars cents =
    toFloat cents / 100


commission : Float
commission =
    0.02


tradeFee : Int
tradeFee =
    0


cessFee : Float
cessFee =
    0.0033


gct : Float
gct =
    0.165


minShares : Int
minShares =
    100


minCommission : Int
minCommission =
    50000



-- The maximum that can be entered without causing integer overflow: 21474836.47


calculateMaxShares : String -> String -> String
calculateMaxShares avaliableCash pricePerStock =
    let
        cash =
            toCents avaliableCash

        price =
            toCents pricePerStock

        maxShares =
            cash // price

        temp =
            fullCal minShares price

        minAmountPrice =
            temp.fin
    in
    if minAmountPrice > cash then
        "Not enought funds to buy shares. "
            ++ "Amount needed to buy "
            ++ String.fromInt minShares
            ++ " shares is $"
            ++ (toDollars minAmountPrice |> String.fromFloat)
            ++ "."

    else
        let
            ans =
                findMaxShares maxShares minShares price cash
        in
        "# of shares: "
            ++ String.fromInt ans.num
            ++ ";     Gross Price: $"
            ++ (String.fromFloat <| toDollars ans.gross)
            ++ ";     Cess Fee: $"
            ++ (String.fromFloat <| toDollars ans.ce)
            ++ ";     Trade Fee: $"
            ++ (String.fromFloat <| toDollars ans.tr)
            ++ ";     GCT: $"
            ++ (String.fromFloat <| toDollars ans.gc)
            ++ ";     Commission: $"
            ++ (String.fromFloat <| toDollars ans.comm)
            ++ ";     Final Price: $"
            ++ (String.fromFloat <| toDollars ans.fin)



-- findMaxShares : Int -> Int -> Int -> Int -> Int


findMaxShares maxShare minShare price cash =
    -- TODO: write function to calculate the max number of shares
    -- stop condition: if (maxShare <= cash) and (maxShare > cash - price)
    let
        priceInfo =
            fullCal maxShare price
    in
    if priceInfo.fin <= cash then
        priceInfo

    else
        findMaxShares (maxShare - 1) minShare price cash



-- fullCal : Int -> Int -> {Int -> Int -> Int -> Int -> Int -> Int}


fullCal numOfShares pricePerStock =
    let
        grossPrice =
            pricePerStock * numOfShares

        tempCommission =
            (toFloat grossPrice * commission) |> truncate

        finalCommission =
            if tempCommission < minCommission then
                minCommission

            else
                tempCommission

        cess =
            (toFloat grossPrice * cessFee) |> truncate

        trade =
            tradeFee

        gctTax =
            toFloat (finalCommission + cess + trade) * gct |> truncate

        finalPrice =
            grossPrice + cess + trade + gctTax + finalCommission
    in
    { gross = grossPrice
    , comm = finalCommission
    , ce = cess
    , tr = trade
    , gc = gctTax
    , fin = finalPrice
    , num = numOfShares
    }

module Main exposing (Model, Msg(..), init, main, update, view)

import Browser
import FormatNumber exposing (format, humanize)
import FormatNumber.Humanize exposing (ZeroStrategy(..))
import FormatNumber.Locales exposing (usLocale)
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
        [ h1 [ style "text-align" "center" ] [ text "Stock Calculator" ]
        , viewInput "text" "total cash" model.avaliableCash AvaliableCash
        , viewInput "text" "price per stock" model.pricePerStock PricePerStock
        , br [] []
        , calculateMaxShares model.avaliableCash model.pricePerStock
        ]


viewInput : String -> String -> String -> (String -> msg) -> Html msg
viewInput t p v toMsg =
    input [ class "uk-input", class "uk-margin-bottom", type_ t, placeholder p, value v, onInput toMsg ] []


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


tradeFee : Float
tradeFee =
    0.0003


cessFee : Float
cessFee =
    0.0033


gct : Float
gct =
    0.165


minShares : Int
minShares =
    1


minCommission : Int
minCommission =
    50000



-- The maximum that can be entered without causing integer overflow: 21,474,836.47


calculateMaxShares : String -> String -> Html msg
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
        div [ style "color" "red" ]
            [ text
                ("Not enough funds to buy a share. "
                    ++ "Amount needed to buy "
                    ++ String.fromInt minShares
                    ++ " share is $"
                    ++ (toDollars minAmountPrice |> format usLocale)
                )
            ]

    else if (cash <= 0) || (price <= 0) then
        div [ style "color" "red" ] [ text "Please enter the price per share." ]

    else
        let
            ans =
                findMaxShares maxShares minShares price cash
        in
        table [ class "uk-table", class "uk-table-divider" ]
            [ tr []
                [ th [] [ text "Text" ]
                , th [] [ text "Amount" ]
                ]
            , tr []
                [ td [] [ text "Number of Shares" ]
                , td [] [ text (humanize usLocale RemoveZeros <| toFloat ans.num) ]
                ]
            , tr []
                [ td [] [ text "Gross Price" ]
                , td [] [ text <| "$ " ++ (format usLocale <| toDollars ans.gross) ]
                ]
            , tr []
                [ td [] [ text "Cess Fee" ]
                , td [] [ text <| "$ " ++ (format usLocale <| toDollars ans.ce) ]
                ]
            , tr []
                [ td [] [ text "Trade Fee" ]
                , td [] [ text <| "$ " ++ (format usLocale <| toDollars ans.tr) ]
                ]
            , tr []
                [ td [] [ text "GCT" ]
                , td [] [ text <| "$ " ++ (format usLocale <| toDollars ans.gc) ]
                ]
            , tr []
                [ td [] [ text "Commission" ]
                , td [] [ text <| "$ " ++ (format usLocale <| toDollars ans.comm) ]
                ]
            , tr []
                [ td [] [ text "Final Price" ]
                , td [] [ text <| "$ " ++ (format usLocale <| toDollars ans.fin) ]
                ]
            , tr []
                [ td [] [ text "Final Price Per Share" ]
                , td [] [ text <| "$ " ++ (format usLocale <| toDollars ans.fpps) ]
                ]
            ]


type alias Info =
    { gross : Int
    , comm : Int
    , ce : Int
    , tr : Int
    , gc : Int
    , fin : Int
    , num : Int
    , fpps : Int
    }


findMaxShares : Int -> Int -> Int -> Int -> Info
findMaxShares maxShare minShare price cash =
    -- TODO: write function to calculate the max number of shares
    -- stop condition: if (maxShare <= cash) and (maxShare > cash - price)
    let
        mid =
            (maxShare + minShare) // 2

        priceInfo =
            fullCal mid price
    in
    if priceInfo.fin == cash then
        priceInfo

    else if (priceInfo.fin < cash) && (priceInfo.fin > (cash - price)) then
        priceInfo

    else if priceInfo.fin > cash then
        findMaxShares (priceInfo.num - 1) minShare price cash

    else
        findMaxShares maxShare (priceInfo.num + 1) price cash


fullCal : Int -> Int -> Info
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
            tradeFee * toFloat grossPrice |> truncate

        gctTax =
            toFloat (finalCommission + cess + trade) * gct |> truncate

        finalPrice =
            grossPrice + cess + trade + gctTax + finalCommission

        finalPricePerShare =
            finalPrice // numOfShares
    in
    { gross = grossPrice
    , comm = finalCommission
    , ce = cess
    , tr = trade
    , gc = gctTax
    , fin = finalPrice
    , num = numOfShares
    , fpps = finalPricePerShare
    }

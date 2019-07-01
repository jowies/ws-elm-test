port module Main exposing (Model, Msg(..), init, main, update, view)

import Browser
import Html exposing (Html, div, h1, img, p, pre, text)
import Html.Attributes exposing (class, src)
import Http
import Json.Decode as Decode
import String



---- MODEL ----


type Info
    = Failure Http.Error
    | Loading
    | Success RealTimeBus


type PhoneCallCalled
    = PhoneCallFailure
    | PhoneCallLoading
    | PhoneCallSuccess PhoneCall


type alias Model =
    { info : Info
    , phoneCall : PhoneCallCalled
    }


type Name
    = Name String


nameDecoder : Decode.Decoder Name
nameDecoder =
    Decode.map Name Decode.string


nameToString : Name -> String
nameToString (Name name) =
    name


type Company
    = Company String


companyDecoder : Decode.Decoder Company
companyDecoder =
    Decode.map Company Decode.string


companyToString : Company -> String
companyToString (Company company) =
    company


type alias PhoneCall =
    { name : Name
    , company : Company
    }


phoneCallDecoder : Decode.Decoder PhoneCall
phoneCallDecoder =
    Decode.map2
        PhoneCall
        (Decode.field "name" nameDecoder)
        (Decode.field "company" companyDecoder)


type Line
    = Line String


lineToString : Line -> String
lineToString (Line line) =
    line


lineDecoder : Decode.Decoder Line
lineDecoder =
    Decode.map Line Decode.string


type Destination
    = Destination String


destinationDecoder : Decode.Decoder Destination
destinationDecoder =
    Decode.map Destination Decode.string


type TimeUntilDeparture
    = TimeUntilDeparture String


timeUntilDepartureDecoder : Decode.Decoder TimeUntilDeparture
timeUntilDepartureDecoder =
    Decode.map TimeUntilDeparture Decode.string


type alias Bus =
    { line : Line
    , destination : Destination
    , timeUntilDeparture : TimeUntilDeparture
    }


busDecoder : Decode.Decoder Bus
busDecoder =
    Decode.map3
        Bus
        (Decode.field "line" lineDecoder)
        (Decode.field "destination" destinationDecoder)
        (Decode.field "timeUntilDeparture" timeUntilDepartureDecoder)


type alias Stop =
    { glos : List Bus
    , prod : List Bus
    }


stopDecoder : Decode.Decoder Stop
stopDecoder =
    Decode.map2 Stop
        (Decode.field "glos" (Decode.list busDecoder))
        (Decode.field "prof" (Decode.list busDecoder))


type alias RealTimeBus =
    { to : Stop
    , from : Stop
    }


realTimeBusDecoder : Decode.Decoder RealTimeBus
realTimeBusDecoder =
    Decode.map2 RealTimeBus
        (Decode.field "to" stopDecoder)
        (Decode.field "from" stopDecoder)


init : ( Model, Cmd Msg )
init =
    let
        initInfo =
            Loading

        initialCommand =
            Http.get
                { url = "https://infoskjerm-api.koskom.no/bus"
                , expect = Http.expectJson GotJson realTimeBusDecoder
                }

        initPhoneCall =
            PhoneCallLoading
    in
    ( Model initInfo initPhoneCall, initialCommand )



---- UPDATE ----


type Msg
    = NoOp
    | GotJson (Result Http.Error RealTimeBus)
    | PhoneCallIn (Result Decode.Error PhoneCall)


errorToString : Http.Error -> String
errorToString error =
    case error of
        Http.BadUrl url ->
            "The URL " ++ url ++ " was invalid"

        Http.Timeout ->
            "Unable to reach the server, try again"

        Http.NetworkError ->
            "Unable to reach the server, check your network connection"

        Http.BadStatus 500 ->
            "The server had a problem, try again later"

        Http.BadStatus 400 ->
            "Verify your information and try again"

        Http.BadStatus _ ->
            "Unknown error"

        Http.BadBody errorMessage ->
            errorMessage


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotJson result ->
            case result of
                Ok fullText ->
                    ( { model | info = Success fullText }, Cmd.none )

                Err err ->
                    Debug.log (errorToString err)
                        ( { model | info = Failure err }, Cmd.none )

        PhoneCallIn result ->
            let
                r =
                    Debug.log "Res" result
            in
            case result of
                Ok fullText ->
                    ( { model | phoneCall = PhoneCallSuccess fullText }, Cmd.none )

                Err err ->
                    Debug.log "Failed"
                        ( { model | phoneCall = PhoneCallFailure }, Cmd.none )

        NoOp ->
            ( model, Cmd.none )


fromValueToMessage : Decode.Value -> Result Decode.Error PhoneCall
fromValueToMessage =
    Decode.decodeValue phoneCallDecoder


subscriptions : Model -> Sub Msg
subscriptions model =
    phoneCallIn (fromValueToMessage >> PhoneCallIn)



---- VIEW ----


view : Model -> Html Msg
view model =
    div []
        [ viewInfo model.info
        , viewPhoneCall model.phoneCall
        ]


viewInfo : Info -> Html Msg
viewInfo info =
    case info of
        Failure err ->
            text (errorToString err)

        Loading ->
            text "Loading..."

        Success json ->
            let
                from =
                    json.from

                glos =
                    from.glos

                line =
                    List.head glos
                        |> Maybe.map (.line >> lineToString)
                        --|> Maybe.map lineToString
                        |> Maybe.withDefault ""
            in
            Html.text (Debug.toString line)


viewPhoneCall : PhoneCallCalled -> Html Msg
viewPhoneCall phoneCall =
    case phoneCall of
        PhoneCallFailure ->
            text "I was unable to load phoneCall."

        PhoneCallLoading ->
            text "Loading..."

        PhoneCallSuccess pc ->
            viewCustomerInfo pc


viewCustomerInfo : PhoneCall -> Html Msg
viewCustomerInfo phone =
    div []
        [ p [] [ text (nameToString phone.name) ]
        , p [] [ text (companyToString phone.company) ]
        ]



--pre [] [ text <| List.head json.to.glos ]
---- PROGRAM ----
--Ports


port phoneCallIn : (Decode.Value -> msg) -> Sub msg


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = subscriptions
        }

port module DBMon exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)


port setState : (List Database -> msg) -> Sub msg


main : Program Never Model Msg
main =
    Html.program
        { view = view
        , update = update
        , init = ( initialModel, Cmd.none )
        , subscriptions = \_ -> setState SetDatabases
        }


type alias Model =
    { databases : List Database }


type alias Database =
    { name : String
    , samples : List { queries : List Query, time : Float }
    }


type alias Query =
    { elapsed : Float
    , query : String
    , canvas_action : Maybe ()
    , canvas_context_id : Maybe ()
    , canvas_controller : Maybe ()
    , canvas_hostname : Maybe ()
    , canvas_job_tag : Maybe ()
    , canvas_pid : Maybe ()
    , elapsed : Float
    , query : String
    , waiting : Bool
    }


type Msg
    = SetDatabases (List Database)


initialModel : { databases : List a }
initialModel =
    { databases = [] }


update : Msg -> Model -> ( Model, Cmd Msg )
update (SetDatabases databases) model =
    ( { model | databases = databases }, Cmd.none )


viewQuery : String -> Float -> Html msg
viewQuery query elapsed =
    let
        className =
            if elapsed >= 10 then
                "elapsed warn_long"
            else if elapsed >= 1 then
                "elapsed warn"
            else
                "elapsed short"

        caption =
            if elapsed > 0 then
                formatElapsed elapsed
            else
                ""
    in
        td [ class ("Query " ++ className) ]
            [ text caption
            , div [ class "popover left" ]
                [ div [ class "popover-content" ] [ text query ]
                , div [ class "arrow" ] []
                ]
            ]


formatElapsed : Float -> String
formatElapsed value =
    if value > 60 then
        let
            minutes =
                value / 60

            comps =
                (toFloat ((floor value) % 60) / 100)
                    |> toString
                    |> String.split "."

            { seconds, ms } =
                case comps of
                    sec :: msec :: _ ->
                        { seconds = String.padLeft 2 '0' sec
                        , ms = msec
                        }

                    _ ->
                        { seconds = "", ms = "" }
        in
            String.join ""
                [ toString minutes, ":", toString seconds, ".", toString ms ]
    else
        toString (toFloat (floor value) / 100)


sample : List Query -> Html msg
sample queries =
    let
        queryCount =
            List.length queries

        countClassName =
            if queryCount >= 20 then
                "label label-important"
            else if queryCount >= 10 then
                "label label-warning"
            else
                "label label-success"
    in
        td [ class "query-count" ]
            (span [ class countClassName ] [ text (toString (List.length queries)) ]
                :: sampleHelp 5 queries []
            )


sampleHelp :
    Int
    -> List Query
    -> List (Html msg)
    -> List (Html msg)
sampleHelp remaining queries results =
    if remaining > 0 then
        case queries of
            [] ->
                (viewQuery "" 0 :: results)
                    |> sampleHelp (remaining - 1) []

            query :: rest ->
                viewQuery query.query query.elapsed
                    :: results
                    |> sampleHelp (remaining - 1) rest
    else
        results


getLastSample : List a -> Maybe a
getLastSample samples =
    case samples of
        [] ->
            Nothing

        last :: [] ->
            Just last

        _ :: rest ->
            getLastSample rest


viewDatabase : Database -> Html msg
viewDatabase database =
    case getLastSample database.samples of
        Just lastSample ->
            tr []
                [ td [ class "dbname" ] [ text database.name ]
                , sample lastSample.queries
                ]

        Nothing ->
            Html.text ""


view : Model -> Html msg
view model =
    div []
        [ table [ class "table table-striped latest-data" ]
            [ tbody [] (List.map viewDatabase model.databases) ]
        ]
